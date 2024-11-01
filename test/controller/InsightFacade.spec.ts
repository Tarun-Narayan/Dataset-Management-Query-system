import {
	IInsightFacade,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	NotFoundError,
	ResultTooLargeError,
} from "../../src/controller/IInsightFacade";
import InsightFacade from "../../src/controller/InsightFacade";
import { clearDisk, getContentFromArchives, loadTestQuery } from "../TestUtil";

import { expect, use } from "chai";
import chaiAsPromised from "chai-as-promised";
import Assertion = Chai.Assertion;

use(chaiAsPromised);

export interface ITestQuery {
	title?: string;
	input: unknown;
	errorExpected: boolean;
	expected: any;
}

describe("InsightFacade", function () {
	let facade: IInsightFacade;

	// Declare datasets used in tests. You should add more datasets like this!
	let sections: string;
	let rooms: string;
	let differentRooms: string;

	before(async function () {
		// This block runs once and loads the datasets.
		sections = await getContentFromArchives("pair.zip");
		rooms = await getContentFromArchives("campus.zip");
		differentRooms = await getContentFromArchives("different_name.zip");

		// Just in case there is anything hanging around from a previous run of the test suite
		await clearDisk();
	});

	describe("AddDataset", function () {
		beforeEach(function () {
			facade = new InsightFacade();
		});
		afterEach(async function () {
			await clearDisk();
		});
		it("rooms should reject with an empty dataset id", function () {
			const result = facade.addDataset("", rooms, InsightDatasetKind.Rooms);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("rooms should add a valid dataset successfully.", async function () {
			const result = await facade.addDataset("ubc", rooms, InsightDatasetKind.Rooms);
			expect(result).to.have.members(["ubc"]);
		});
		it("rooms should add a valid dataset with a different name successfully ", async function () {
			const result = await facade.addDataset("id", differentRooms, InsightDatasetKind.Rooms);
			expect(result).to.have.members(["id"]);
		});

		it("rooms should reject a zip with missing index.htm", async function () {
			const result = facade.addDataset(
				"id",
				await getContentFromArchives("campus_missing_index.zip"),
				InsightDatasetKind.Rooms
			);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});

		it("rooms should reject when index.htm is missing buildings table", async function () {
			const result = facade.addDataset(
				"id",
				await getContentFromArchives("index_missing_table.zip"),
				InsightDatasetKind.Rooms
			);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});

		it("should reject with an empty dataset id", function () {
			const result = facade.addDataset("", sections, InsightDatasetKind.Sections);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});

		it("should reject with an id that is only whitespace", function () {
			const result = facade.addDataset(" ", sections, InsightDatasetKind.Sections);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with an id with an underscore", function () {
			const result = facade.addDataset("ubc_", sections, InsightDatasetKind.Sections);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with an id already saved", async function () {
			await facade.addDataset("id", sections, InsightDatasetKind.Sections);

			try {
				await facade.addDataset("id", sections, InsightDatasetKind.Sections);
				expect.fail("should have rejected");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});
		it("should reject with an invalid Kind argument", function () {
			const result = facade.addDataset("id", sections, InsightDatasetKind.Rooms);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with an invalid Content file argument", async function () {
			const result = facade.addDataset(
				"id",
				await getContentFromArchives("emptydataset.zip"),
				InsightDatasetKind.Sections
			);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with a Content file with no valid sections", async function () {
			const result = facade.addDataset(
				"id",
				await getContentFromArchives("emptysection.zip"),
				InsightDatasetKind.Sections
			);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with an invalid Content argument", function () {
			const result = facade.addDataset("id", "", InsightDatasetKind.Sections);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with folder not named courses in Content", async function () {
			const result = facade.addDataset(
				"id",
				await getContentFromArchives("foldername.zip"),
				InsightDatasetKind.Sections
			);
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with an invalid course file type", async function () {
			try {
				await facade.addDataset("invalidCourseFile", "invalid_course_file_type.zip", InsightDatasetKind.Sections);
				expect.fail("Should have thrown error");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});
		it("should reject with an invalid file type (.txt)", async function () {
			try {
				await facade.addDataset("invalidTxtFile", "text_file.txt", InsightDatasetKind.Sections);
				expect.fail("Should have thrown error");
			} catch (err) {
				expect(err).to.be.instanceOf(InsightError);
			}
		});

		it("should add a valid dataset successfully.", async function () {
			const result = await facade.addDataset("ubc", sections, InsightDatasetKind.Sections);
			expect(result).to.have.members(["ubc"]);
		});
		it("should accept and save more than one valid dataset", async function () {
			const first = await facade.addDataset("ubc", sections, InsightDatasetKind.Sections);
			const second = await facade.addDataset("id", sections, InsightDatasetKind.Sections);

			expect(first).to.have.members(["ubc"]);
			expect(second).to.have.members(["ubc", "id"]);
		});
	});

	describe("RemoveDataset", function () {
		beforeEach(function () {
			facade = new InsightFacade();
		});
		afterEach(async function () {
			await clearDisk();
		});

		it("should reject an empty dataset id", function () {
			const result = facade.removeDataset("");
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject an id that is only whitespace", function () {
			const result = facade.removeDataset(" ");
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject an id with an underscore", function () {
			const result = facade.removeDataset("ubc_");
			return expect(result).to.eventually.be.rejectedWith(InsightError);
		});
		it("should reject with a valid id not already saved", function () {
			const result = facade.removeDataset("ubc");
			return expect(result).to.eventually.be.rejectedWith(NotFoundError);
		});

		it("should remove a valid dataset successfully.", async function () {
			const first = await facade.addDataset("ubc", sections, InsightDatasetKind.Sections);
			const second = await facade.removeDataset("ubc");

			expect(first).to.have.members(["ubc"]);
			expect(second).to.eq("ubc");

			const datasets = await facade.listDatasets();
			expect(datasets).to.deep.equal([]);
		});
	});

	describe("ListDataset", function () {
		beforeEach(function () {
			facade = new InsightFacade();
		});
		afterEach(async function () {
			await clearDisk();
		});

		it("should list one dataset", async function () {
			await facade.addDataset("ubc", rooms, InsightDatasetKind.Rooms);

			const datasets = await facade.listDatasets();
			expect(datasets).to.deep.equal([
				{
					id: "ubc",
					kind: InsightDatasetKind.Rooms,
					numRows: 364,
				},
			]);
		});
	});

	describe("PerformQuery", function () {
		/**
		 * Loads the TestQuery specified in the test name and asserts the behaviour of performQuery.
		 *
		 * Note: the 'this' parameter is automatically set by Mocha and contains information about the test.
		 */

		async function checkQuery(this: Mocha.Context): Promise<Assertion> {
			if (!this.test) {
				throw new Error(
					"Invalid call to checkQuery." +
						"Usage: 'checkQuery' must be passed as the second parameter of Mocha's it(..) function." +
						"Do not invoke the function directly."
				);
			}
			// Destructuring assignment to reduce property accesses
			const { input, expected, errorExpected } = await loadTestQuery(this.test.title);
			let result: InsightResult[];
			try {
				result = await facade.performQuery(input);
				//const number = 2;

				// Log actual and expected results for comparison
				// console.log("Actual result: ", JSON.stringify(result, null, number));
				// console.log("Expected result: ", JSON.stringify(expected, null, number));
				if (errorExpected) {
					expect.fail(`performQuery resolved when it should have rejected with ${expected}`);
				}
				return expect(result).to.deep.equal(expected);
			} catch (err) {
				if (!errorExpected) {
					expect.fail(`performQuery threw unexpected error: ${err}`);
				}
				if (expected === "InsightError") {
					return expect(err).to.be.instanceOf(InsightError);
				}
				if (expected === "ResultTooLargeError") {
					return expect(err).to.be.instanceOf(ResultTooLargeError);
				}
				expect.fail("should not have reached");
			}
		}

		before(async function () {
			facade = new InsightFacade();

			// Add the datasets to InsightFacade once.
			// Will *fail* if there is a problem reading ANY dataset.
			const loadDatasetPromises: Promise<string[]>[] = [
				facade.addDataset("sections", sections, InsightDatasetKind.Sections),
				facade.addDataset("rooms", rooms, InsightDatasetKind.Rooms),
			];

			try {
				await Promise.all(loadDatasetPromises);
			} catch (err) {
				throw new Error(`In PerformQuery Before hook, dataset(s) failed to be added. \n${err}`);
			}
		});

		after(async function () {
			await clearDisk();
		});

		// Examples demonstrating how to test performQuery using the JSON Test Queries.
		// The relative path to the query file must be given in square brackets.
		//
		it("[valid/simple.json] SELECT dept, avg WHERE avg > 97", checkQuery);
		it("[valid/wildcard_first.json] Query with wildcard put first", checkQuery);
		it("[valid/wildcard_last.json] Query with wildcard put last", checkQuery);
		it("[valid/wildcard_contains.json] Query with wildcard put both sides", checkQuery);
		it("[valid/not.json] Query using not", checkQuery);
		it("[valid/or.json] Query using or logic", checkQuery);
		it("[valid/equal.json] Query using equal comparator", checkQuery);
		it("[valid/complex.json] Very complex valid query", checkQuery);
		it("[valid/sComparison_test.json] SELECT sections WHERE instructor contains 'pres'", checkQuery);
		it("[valid/rooms.json] Query using ROOMS section", checkQuery);
		it("[valid/empty_input.json] Test empty input to query", checkQuery);
		it("[valid/empty_result.json] Test empty result to query", checkQuery);
		it("[valid/transformation.json] Query using a complex transformation", checkQuery);
		it("[valid/transformation_max.json] Query using a complex transformation MAX", checkQuery);
		it("[valid/transformation_avg.json] Query using a complex transformation AVG", checkQuery);
		it("[valid/transformation_min.json] Query using a complex transformation MIN", checkQuery);
		it("[valid/transformation_sum.json] Query using a complex transformation SUM", checkQuery);
		it("[valid/transformation_count.json] Query using a complex transformation COUNT", checkQuery);
		it("[valid/sort_down.json] Sort down", checkQuery);
		it("[valid/sort_up.json] Sort up", checkQuery);
		it("[valid/single_and.json] Query using AND logic with one filter", checkQuery);
		it("[valid/single_or.json] Query using OR logic with one filter", checkQuery);
		it("[valid/transformation_all.json] Query with all group transform", checkQuery);
		it("[valid/transformation_multiple.json] Query using multiple transformations", checkQuery);

		it("[invalid/invalid.json] Query missing WHERE", checkQuery);
		it("[invalid/asterisk_middle.json] Query has asterisk in invalid spot", checkQuery);
		it("[invalid/not_object.json] Query is not an object", checkQuery);
		it("[invalid/multiple.json] Query references multiple ids", checkQuery);
		it("[invalid/too_large.json] Query result is too large", checkQuery);
		it("[invalid/order_invalid.json] Order key is not in Columns", checkQuery);
		it("[invalid/invalid_key.json] Query key cannot be empty string", checkQuery);
		it("[invalid/invalid_key2.json] Query key is invalid", checkQuery);
		it("[invalid/missing_options.json] Query missing options", checkQuery);
		it("[invalid/missing_columns.json] Query missing columns", checkQuery);
		it("[invalid/missing_keylist.json] Query missing keylist", checkQuery);
		it("[invalid/invalid_comparator.json] Query comparator needs to be number", checkQuery);
		it("[invalid/invalid_logic.json] Query logic is not valid", checkQuery);
		it("[invalid/no_match.json] Dataset has not been added yet", checkQuery);
		it("[invalid/comparator_invalid.json] Comparator is not real", checkQuery);
		it("[invalid/not_mkey.json] Key used is skey", checkQuery);
		it("[invalid/not_skey.json] Key used is mkey", checkQuery);
		it("[invalid/missing_filterlist.json] Query missing filterlist", checkQuery);
		it("[invalid/missing_inputstring.json] Query input string is wrong type", checkQuery);
	});
});
