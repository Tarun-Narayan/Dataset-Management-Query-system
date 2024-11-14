import { expect } from "chai";
import request, { Response } from "supertest";
import { StatusCodes } from "http-status-codes/build/cjs/status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import Server from "../../src/rest/Server";
import { clearDisk } from "../TestUtil";
require("supertest");

describe("Facade C3", function () {
	let server: Server;

	before(async function () {
		server = new Server(0);
		await server.start();
		await clearDisk();
	});

	after(async function () {
		await server.stop();
	});

	describe("PUT", function () {
		// Sample on how to format PUT requests
		it("PUT test for adding one Sections dataset", async function () {
			const SERVER_URL = server.getServer();
			const ENDPOINT_URL = "/dataset/ubc/sections";
			const ZIP_FILE_DATA = Buffer.from("pair.zip");

			try {
				return request(SERVER_URL)
					.put(ENDPOINT_URL)
					.send(ZIP_FILE_DATA)
					.set("Content-Type", "application/x-zip-compressed")
					.then(function (res: Response) {
						// some logging here please!
						expect(res.status).to.be.equal(StatusCodes.OK);
					})
					.catch(function (err) {
						// some logging here please!
						Log.info(err);
						expect.fail();
					});
			} catch (err) {
				Log.error(err);
				// and some more logging here!
			}
		});
	});

	// The other endpoints work similarly. You should be able to find all instructions in the supertest documentation
});
