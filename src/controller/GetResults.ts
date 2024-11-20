import { InsightDatasetKind, InsightError, InsightResult, ResultTooLargeError } from "./IInsightFacade";
import path from "path";
import * as fs from "fs-extra";
import { Order, Query } from "./Query";
import { getResultObject, orderResults, orderResultsSingle, resultsMap } from "./GetResultObject";
import { getSections } from "./GetResultHelpers";
import { handleTransformations } from "./Transformations";

const MAX_SIZE = 5000;

export async function getResults(query: Query): Promise<InsightResult[]> {
	const dataset = await getDataset(query);
	const sections = await getSections(query.WHERE, dataset);

	let objects = await getResultObject(query.OPTIONS, Array.from(sections));
	if (query.TRANSFORMATIONS) {
		objects = await handleTransformations(query.TRANSFORMATIONS, query.OPTIONS.COLUMNS);
		if (typeof query.OPTIONS.ORDER === "string") {
			const orderKey = query.OPTIONS.ORDER; // The column name to order by
			objects = orderResultsSingle(objects, orderKey); // Use the helper function to order results
		} else if (query.OPTIONS.ORDER) {
			const orderKeys = (query.OPTIONS.ORDER as Order).keys;
			const dir = (query.OPTIONS.ORDER as Order).dir;
			objects = orderResults(objects, orderKeys, dir);
		}
	}

	if (objects.length > MAX_SIZE) {
		throw new ResultTooLargeError(`Query Result size exceeded: ` + `${MAX_SIZE}`);
	}

	resultsMap.clear();
	return objects;
}

export async function getDataset(
	query: Query
): Promise<{ id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }> {
	try {
		let keyToParse;
		if (query.TRANSFORMATIONS) {
			keyToParse = await Promise.any(
				query.TRANSFORMATIONS.GROUP.map((key) => {
					if (key.includes("_")) {
						return key;
					}
				})
			);
		} else {
			keyToParse = query.OPTIONS.COLUMNS[0];
		}
		if (!keyToParse) {
			throw new InsightError("Did not locate reference ID");
		}
		const id = keyToParse.split("_", 1)[0];

		const filePath = path.join("./data", `${id}.json`);
		return await fs.readJson(filePath);
	} catch {
		throw new InsightError("Referenced dataset ID had not been added yet");
	}
}
