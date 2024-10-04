import { InsightResult } from "./IInsightFacade";
import { Options } from "./Query";

export async function getResultObject(options: Options, sections: any[]): Promise<InsightResult[]> {
	// Create an array to hold the results
	const results: InsightResult[] = [];

	for (const section of sections) {
		// Create an object to hold the relevant columns for the current section
		const resultEntry: any = {};

		// Extract the columns specified in options.COLUMNS
		for (const column of options.COLUMNS) {
			if (Object.prototype.hasOwnProperty.call(section, column)) {
				resultEntry[column] = section[column];
			}
		}

		// Add the filtered section object to the results array
		results.push(resultEntry);
	}

	// If options.ORDER is provided, sort the results based on that column
	if (options.ORDER) {
		const field = options.ORDER as string;
		results.sort((a, b) => {
			const aValue = a[field];
			const bValue = b[field];

			// Extract the numeric part of the field
			const aNumericValue = typeof aValue === "string" ? parseFloat(aValue.split(":")[1].trim()) : aValue;

			const bNumericValue = typeof bValue === "string" ? parseFloat(bValue.split(":")[1].trim()) : bValue;

			if (!isNaN(aNumericValue) && !isNaN(bNumericValue)) {
				return aNumericValue - bNumericValue; // Sort in ascending order
			} else {
				return 0; // If not both valid numbers, do not change the order
			}
		});
	}
	return results;
}
