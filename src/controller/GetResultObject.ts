import { InsightError, InsightResult } from "./IInsightFacade";
import { Options, Order } from "./Query";

// Data file to dataset mapping.
export const resultsMap = new Map<InsightResult, any>();
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
	lat: "lat",
	lon: "lon",
	seats: "seats",
	fullname: "fullname",
	shortname: "shortname",
	number: "number",
	name: "name",
	address: "address",
	type: "type",
	furniture: "furniture",
	href: "href",
};

// Map section keys to result keys
function mapSectionToResult(section: any): any {
	const resultEntry: any = {};

	for (const key of Object.keys(section)) {
		const mappedKey = mapping[key];
		if (mappedKey) {
			// Set Year to 1900 if Section is "overall"
			if (key === "Year") {
				if (section.Section === "overall") {
					resultEntry[mappedKey] = 1900;
				} else {
					const yearValue = section[key];
					// Convert to number, year is string currently
					resultEntry[mappedKey] = yearValue !== "" ? Number(yearValue) : undefined;
				}
			}
			// Handle UUID to convert to a string
			else if (key === "id") {
				const uuidValue = section[key];
				resultEntry[mappedKey] = uuidValue !== "" ? String(uuidValue) : undefined; // Convert to string, add quotes
			} else {
				resultEntry[mappedKey] = section[key];
			}
		}
	}
	resultsMap.set(resultEntry, section);
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

// Order results based on the specified key
export function orderResultsSingle(results: InsightResult[], orderKey: string): InsightResult[] {
	return results.sort((a, b) => {
		const aValue = a[orderKey];
		const bValue = b[orderKey];
		// Check if both values are numbers
		if (typeof aValue === "number" && typeof bValue === "number") {
			return aValue - bValue;
		} else {
			// String comparison
			const aString = String(aValue);
			const bString = String(bValue);
			if (aString < bString) {
				return -1; //Ascending
			}
			if (aString > bString) {
				return 1; //Descending
			}
			return 0;
		}
	});
}

function orderStringResult(aString: string, bString: string, dir: string): number {
	if (dir === "UP") {
		if (aString < bString) {
			return -1; //Ascending
		}
		if (aString > bString) {
			return 1; //Descending
		}
	} else {
		if (aString > bString) {
			return -1; //Ascending
		}
		if (aString < bString) {
			return 1; //Descending
		}
	}
	return 0;
}

export function orderResults(results: InsightResult[], orderKeys: string[], dir: string): InsightResult[] {
	return results.sort((a, b) => {
		for (const key of orderKeys) {
			const aValue = a[key];
			const bValue = b[key];
			if (typeof aValue === "number" && typeof bValue === "number") {
				if (aValue - bValue !== 0) {
					if (dir === "UP") {
						return aValue - bValue;
					} else {
						return bValue - aValue;
					}
				}
			} else {
				// String comparison
				const aString = String(aValue);
				const bString = String(bValue);
				const result = orderStringResult(aString, bString, dir);
				if (result !== 0) {
					return result;
				}
			}
		}
		return 0;
	});
}

// Get the result object
export async function getResultObject(options: Options, sections: any[]): Promise<InsightResult[]> {
	let results: InsightResult[] = [];

	if (!validKeys(options.COLUMNS)) {
		throw new InsightError("Query references multiple datasets");
	}

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

	// Order the results based on options.ORDER (string column name)
	if (typeof options.ORDER === "string") {
		const orderKey = options.ORDER; // The column name to order by
		results = orderResultsSingle(results, orderKey); // Use the helper function to order results
	} else if (options.ORDER) {
		const orderKeys = (options.ORDER as Order).keys;
		const dir = (options.ORDER as Order).dir;
		results = orderResults(results, orderKeys, dir);
	}

	return results;
}

function validKeys(keys: string[]): boolean {
	const keyToParse = keys.find((element) => element.includes("_"));
	if (keyToParse) {
		const validID = keyToParse.split("_")[0];

		for (const key of keys) {
			if (key.includes("_")) {
				const toCompare = key.split("_")[0];
				if (toCompare !== validID) {
					return false;
				}
			}
		}
	}
	return true;
}
