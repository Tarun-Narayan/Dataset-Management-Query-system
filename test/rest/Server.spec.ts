import { expect } from "chai";
import request, { Response } from "supertest";
import { StatusCodes } from "http-status-codes/build/cjs/status-codes";
import Log from "@ubccpsc310/folder-test/build/Log";
import Server from "../../src/rest/Server";
import { clearDisk } from "../TestUtil";

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
			const SERVER_URL = "http://localhost:0";
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

	describe("POST", function () {
		it("POST test for querying a dataset", async function () {
			const SERVER_URL = server.getServer();
			const ENDPOINT_URL = "/query";
			const QUERY = {
				WHERE: {
					OR: [
						{
							EQ: {
								ubc_avg: 50,
							},
						},
						{
							IS: {
								ubc_id: "1000",
							},
						},
						{
							NOT: {
								LT: {
									ubc_year: 2020,
								},
							},
						},
					],
				},
				OPTIONS: {
					COLUMNS: [
						"ubc_dept",
						"ubc_avg",
						"ubc_pass",
						"ubc_fail",
						"ubc_audit",
						"ubc_year",
						"ubc_id",
						"ubc_uuid",
						"ubc_title",
						"ubc_instructor",
					],
					ORDER: "ubc_instructor",
				},
			};

			try {
				return request(SERVER_URL)
					.post(ENDPOINT_URL)
					.send(QUERY)
					.set("Content-Type", "application/json")
					.then(function (res: Response) {
						Log.info("POST request response status: " + res.status);
						expect(res.status).to.be.equal(StatusCodes.OK);
					})
					.catch(function (err) {
						Log.info("POST request error: " + err);
						expect.fail();
					});
			} catch (err) {
				Log.error("Unexpected error in POST test: " + err);
			}
		});
	});

	describe("DELETE", function () {
		it("DELETE test for removing a dataset", async function () {
			const SERVER_URL = server.getServer();
			const ENDPOINT_URL = "/dataset/ubc";

			try {
				return request(SERVER_URL)
					.delete(ENDPOINT_URL)
					.then(function (res: Response) {
						Log.info("DELETE request response status: " + res.status);
						expect(res.status).to.be.equal(StatusCodes.OK);
					})
					.catch(function (err) {
						Log.info("DELETE request error: " + err);
						expect.fail();
					});
			} catch (err) {
				Log.error("Unexpected error in DELETE test: " + err);
			}
		});
	});

	describe("GET", function () {
		it("GET test for listing datasets", async function () {
			const SERVER_URL = server.getServer();
			const ENDPOINT_URL = "/datasets";

			try {
				return request(SERVER_URL)
					.get(ENDPOINT_URL)
					.then(function (res: Response) {
						Log.info("GET request response status: " + res.status);
						expect(res.status).to.be.equal(StatusCodes.OK);
						expect(res.body).to.have.property("result");
					})
					.catch(function (err) {
						Log.info("GET request error: " + err);
						expect.fail();
					});
			} catch (err) {
				Log.error("Unexpected error in GET test: " + err);
			}
		});
	});
	// The other endpoints work similarly. You should be able to find all instructions in the supertest documentation
});
