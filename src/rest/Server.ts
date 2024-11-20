import express, { Application, Request, Response } from "express";
import { StatusCodes } from "http-status-codes/build/cjs/status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import * as http from "http";
import cors from "cors";
import InsightFacade from "../controller/InsightFacade";
import fs from "fs/promises";
import path from "path";

import { InsightDatasetKind, InsightError, NotFoundError } from "../controller/IInsightFacade";

const kindMap: Record<string, InsightDatasetKind> = {
	section: InsightDatasetKind.Sections,
	room: InsightDatasetKind.Rooms,
};

export default class Server {
	private readonly port: number;
	private express: Application;
	private server: http.Server | undefined;
	private static facade: InsightFacade;

	constructor(port: number) {
		Log.info(`Server::<init>( ${port} )`);
		this.port = port;
		this.express = express();
		Server.facade = new InsightFacade();

		this.registerMiddleware();
		this.registerRoutes();
		Server.facade = new InsightFacade();

		// NOTE: you can serve static frontend files in from your express server
		// by uncommenting the line below. This makes files in ./frontend/public
		// accessible at http://localhost:<port>/
		this.express.use(express.static("./frontend/public"));
	}

	/**
	 * Starts the server. Returns a promise that resolves if success. Promises are used
	 * here because starting the server takes some time, and we want to know when it
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
	 * Stops the server. Again returns a promise, so we know when the connections have
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
		// JSON parser must be in place before raw parser because of wildcard matching done by raw parser below
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
			// Parse fields
			const id = req.params.id;
			const kindString = req.params.kind.toLowerCase();
			const kind = kindMap[kindString];

			// Validate the kind
			if (!kind) {
				Log.error("Inputted kind type does not exist");
				res.status(StatusCodes.BAD_REQUEST).json({ error: "Inputted kind type does not exist" });
				return;
			}

			// Validate the file
			if (!req.body || !(req.body instanceof Buffer)) {
				Log.error("File data not received or not in the correct format");
				res.status(StatusCodes.BAD_REQUEST).json({ error: "File data is missing or invalid" });
				return;
			}

			// Convert the file buffer to Base64
			const content = req.body.toString("base64");

			// Process the dataset
			const result = await Server.facade.addDataset(id, content, kind);
			Log.info(`Dataset: '${id}' has been successfully added`);
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			Log.error(`${err}`);
			res.status(StatusCodes.BAD_REQUEST).json({ error: `${err}` });
		}
	}

	private static async remove(req: Request, res: Response): Promise<void> {
		try {
			const id = req.params.id;
			const result = await Server.facade.removeDataset(id);
			Log.info(`Dataset: '${id}' has been successfully removed`);

			// Save the removed dataset ID to a file
			const filePath = path.resolve(__dirname, "../../frontend/public/removed-datasets.json");
			const num = 2;
			try {
				const data = await fs.readFile(filePath, "utf-8");
				const removedList = JSON.parse(data);
				removedList.push(id);
				await fs.writeFile(filePath, JSON.stringify(removedList, null, num));
			} catch (error) {
				// @ts-ignore
				if (error.code === "ENOENT") {
					await fs.writeFile(filePath, JSON.stringify([id], null, num));
				} else {
					throw error;
				}
			}

			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			if (err instanceof NotFoundError) {
				Log.error(`Dataset: '${req.params.id}' not found`);
				res.status(StatusCodes.NOT_FOUND).json({ error: `${err}` });
			} else if (err instanceof InsightError) {
				Log.error(`Error removing dataset: ${err}`);
				res.status(StatusCodes.BAD_REQUEST).json({ error: `${err}` });
			} else {
				Log.error(`Unexpected error: ${err}`);
				res.status(StatusCodes.BAD_REQUEST).json({ error: `${err}` });
			}
		}
	}

	private static async performQ(req: Request, res: Response): Promise<void> {
		try {
			const query = req.body;
			Log.info(query);
			const result = await Server.facade.performQuery(query);
			Log.info(`Query has been successfully performed`);
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			Log.error(`${err}`);
			res.status(StatusCodes.BAD_REQUEST).json({ error: `${err}` });
		}
	}

	private static async list(_req: Request, res: Response): Promise<void> {
		try {
			const result = await Server.facade.listDatasets();
			Log.info("List of datasets retrieved successfully");
			res.status(StatusCodes.OK).json({ result: result });
		} catch (err) {
			Log.error(`Error listing datasets: ${err}`);
			res.status(StatusCodes.BAD_REQUEST).json({ error: `${err}` });
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
}
