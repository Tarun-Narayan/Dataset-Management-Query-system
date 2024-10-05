import { InsightResult } from "./IInsightFacade";
import { Options } from "./Query";

// Data file to dataset mapping.
const mapping: Record<string, string> = {
	id: "uuid",
	Course: "id",
	Title: "title",
	Professor: "instructor",
	Subject: "dept",
	Year: "year",
	Avg: "avg",
	Pass: "pass",
	Fail: "fail",
	Audit: "audit",
};

// Map section keys to result keys
function mapSectionToResult(section: any): any {
	const resultEntry: any = {};

	for (const key of Object.keys(section)) {
		const mappedKey = mapping[key];
		if (mappedKey) {
			resultEntry[mappedKey] = section[key];
		}
	}

	return resultEntry;
}

// Filter out keys not in COLUMNS
function filterResultKeys(resultEntry: any, columns: string[]): any {
	const filteredEntry: any = {};
	for (const key of Object.keys(resultEntry)) {
		const normalizedKey = key.toLowerCase();
		let matchFound = false;

		for (const column of columns) {
			const normalizedColumn = column.split("_").pop()?.toLowerCase();
			if (normalizedKey === normalizedColumn) {
				matchFound = true;
				break;
			}
		}

		if (matchFound) {
			filteredEntry[key] = resultEntry[key];
		}
	}
	return filteredEntry;
}

// Get the result object
export async function getResultObject(options: Options, sections: any[]): Promise<InsightResult[]> {
	const results: InsightResult[] = [];

	for (const section of sections) {
		const mappedResult = mapSectionToResult(section); // Map section keys to result keys
		const filteredResult = filterResultKeys(mappedResult, options.COLUMNS); // Filter based on COLUMNS

		// Rename keys to include 'DatsetName_' prefix
		const finalEntry: any = {};
		for (const column of options.COLUMNS) {
			for (const key of Object.keys(filteredResult)) {
				if (key.toLowerCase() === column.split("_").pop()?.toLowerCase()) {
					finalEntry[column] = filteredResult[key];
				}
			}
		}

		results.push(finalEntry); // Add the final entry to results
	}
	return results;
}
