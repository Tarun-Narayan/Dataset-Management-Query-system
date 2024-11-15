import express, { Application, Request, Response } from "express";
import { StatusCodes } from "http-status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import * as http from "http";
import cors from "cors";
import InsightFacade from "../controller/InsightFacade";
import { InsightDatasetKind, InsightError, NotFoundError } from "../controller/IInsightFacade";
import { getContentFromArchives } from "../../test/TestUtil";

export default class Server {
	private readonly port: number;
	private express: Application;
	private server: http.Server | undefined;
	private static facade: InsightFacade;
	constructor(port: number) {
		Log.info(`Server::<init>( ${port} )`);
		this.port = port;
		this.express = express();

		this.registerMiddleware();
		this.registerRoutes();
		Server.facade = new InsightFacade();

		// NOTE: you can serve static frontend files in from your express server
		// by uncommenting the line below. This makes files in ./frontend/public
		// accessible at http://localhost:<port>/
		// this.express.use(express.static("./frontend/public"))
	}

	/**
	 * Starts the server. Returns a promise that resolves if success. Promises are used
	 * here because starting the server takes some time and we want to know when it
	 * is done (and if it worked).
	 *
	 * @returns {Promise<void>}
	 */
	public async start(): Promise<void> {
		return new Promise((resolve, reject) => {
			Log.info("Server::start() - start");
			if (this.server !== undefined) {
				Log.error("Server::start() - server already listening");
				reject();
			} else {
				this.server = this.express
					.listen(this.port, () => {
						Log.info(`Server::start() - server listening on port: ${this.port}`);
						resolve();
					})
					.on("error", (err: Error) => {
						// catches errors in server start
						Log.error(`Server::start() - server ERROR: ${err.message}`);
						reject(err);
					});
			}
		});
	}

	/**
	 * Stops the server. Again returns a promise so we know when the connections have
	 * actually been fully closed and the port has been released.
	 *
	 * @returns {Promise<void>}
	 */
	public async stop(): Promise<void> {
		Log.info("Server::stop()");
		return new Promise((resolve, reject) => {
			if (this.server === undefined) {
				Log.error("Server::stop() - ERROR: server not started");
				reject();
			} else {
				this.server.close(() => {
					Log.info("Server::stop() - server closed");
					resolve();
				});
			}
		});
	}

	// Registers middleware to parse request before passing them to request handlers
	private registerMiddleware(): void {
		// JSON parser must be place before raw parser because of wildcard matching done by raw parser below
		this.express.use(express.json());
		this.express.use(express.raw({ type: "application/*", limit: "10mb" }));

		// enable cors in request headers to allow cross-origin HTTP requests
		this.express.use(cors());
	}

	// Registers all request handlers to routes
	private registerRoutes(): void {
		// This is an example endpoint this you can invoke by accessing this URL in your browser:
		// http://localhost:4321/echo/hello
		this.express.get("/echo/:msg", Server.echo);
		this.express.put("/dataset/:id/:kind", Server.add);
		this.express.delete("/dataset/:id", Server.remove);
		this.express.post("/query", Server.performQ);
		this.express.get("/datasets", Server.list);
	}

	private static async add(req: Request, res: Response): Promise<void> {
		try {
			// parse fields
			const id = req.params.id;
			const kindString = req.params.kind.toLowerCase();
			const content = await getContentFromArchives(req.body.toString());

			// retrieve result
			let result: string[];
			if (kindString === "rooms") {
				result = await Server.facade.addDataset(id, content, InsightDatasetKind.Rooms);
			} else if (kindString === "sections") {
				result = await Server.facade.addDataset(id, content, InsightDatasetKind.Sections);
			} else {
				// return error for invalid kind
				Log.error("Inputted kind type does not exist");
				res.status(StatusCodes.BAD_REQUEST).json({ error: "Inputted kind type does not exist" });
				return Promise.resolve();
			}

			// return valid result
			Log.info("Dataset: '" + id + "' has been successfully added");
			res.status(StatusCodes.OK).json({ result: result });
			return Promise.resolve();
		} catch (err) {
			// return all other errors
			Log.error(err);
			res.status(StatusCodes.BAD_REQUEST).json({ error: err });
			return Promise.reject();
		}
	}

	private static async remove(req: Request, res: Response): Promise<void> {
		try {
			const id = req.params.id;
			const result = await Server.facade.removeDataset(id);
			Log.info(`Dataset: '${id}' has been successfully removed`);
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			const id = req.params.id;
			if (err instanceof NotFoundError) {
				Log.error(`Dataset: '${id}' not found`);
				res.status(StatusCodes.NOT_FOUND).json({ error: err });
			} else if (err instanceof InsightError) {
				Log.error(`Error removing dataset: ${err}`);
				res.status(StatusCodes.BAD_REQUEST).json({ error: err });
			} else {
				Log.error(`Unexpected error: ${err}`);
				res.status(StatusCodes.BAD_REQUEST).json({ error: err });
			}
		}
	}

	private static async performQ(req: Request, res: Response): Promise<void> {
		try {
			const query = req.body;
			const result = await Server.facade.performQuery(query);
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			res.status(StatusCodes.BAD_REQUEST).json({ error: err });
		}
	}

	private static async list(_req: Request, res: Response): Promise<void> {
		try {
			const result = await Server.facade.listDatasets();
			Log.info("List of datasets retrieved successfully");
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			Log.error(`Error listing datasets: ${err}`);
			res.status(StatusCodes.BAD_REQUEST).json({ error: err });
		}
	}

	// The next two methods handle the echo service.
	// These are almost certainly not the best place to put these, but are here for your reference.
	// By updating the Server.echo function pointer above, these methods can be easily moved.
	private static echo(req: Request, res: Response): void {
		try {
			Log.info(`Server::echo(..) - params: ${JSON.stringify(req.params)}`);
			const response = Server.performEcho(req.params.msg);
			res.status(StatusCodes.OK).json({ result: response });
		} catch (err) {
			res.status(StatusCodes.BAD_REQUEST).json({ error: err });
		}
	}

	private static performEcho(msg: string): string {
		if (typeof msg !== "undefined" && msg !== null) {
			return `${msg}...${msg}`;
		} else {
			return "Message not provided";
		}
	}
	public getServer(): http.Server | undefined {
		return this.server;
	}
}
