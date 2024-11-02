import JSZip from "jszip";
import { InsightDatasetKind, InsightError } from "./IInsightFacade";
import * as fs from "fs-extra";
import * as path from "path";
import { parseRoomsZipFile } from "./RoomsAddHelpers";

// Path for storing datasets
const DATASET_DIRECTORY = "./data";

// Validate the dataset based on rules for a valid UBC course dataset.
export function validateDataset(id: string, kind: InsightDatasetKind): void {
	if (id.trim().length === 0 || id.includes("_")) {
		throw new InsightError("Invalid ID: ID is empty, only whitespace, or contains an underscore.");
	}
	if (kind !== InsightDatasetKind.Sections && kind !== InsightDatasetKind.Rooms) {
		throw new InsightError("Invalid Dataset Kind: Only 'Sections' and 'Rooms' is supported.");
	}
}

//Parse the zip file: either rooms or sections
export async function parseZipFile(content: string, kind: InsightDatasetKind): Promise<Map<string, any>> {
	const zip = new JSZip();
	let zipContent: JSZip;

	// Check if the content is a valid zip file
	try {
		zipContent = await zip.loadAsync(content, { base64: true });
	} catch (_error) {
		throw new InsightError("Invalid Zip File!");
	}

	if (kind === InsightDatasetKind.Rooms) {
		return await parseRoomsZipFile(zipContent);
	} else if (kind === InsightDatasetKind.Sections) {
		return await parseSectionsZipFile(zipContent);
	} else {
		throw new InsightError("Unsupported dataset kind!");
	}
}
// Function to parse sections dataset
async function parseSectionsZipFile(zipContent: JSZip): Promise<Map<string, any>> {
	const fileMap = new Map<string, any>();

	// Check if the zip file contains the "courses" folder
	if (!zipContent.files?.["courses/"]) {
		throw new InsightError("'courses' folder is missing in the zip file!");
	}

	// Parse files in the courses folder
	await Promise.all(
		Object.keys(zipContent.files).map(async (fileName) => {
			const file = zipContent.files[fileName];

			if (fileName.startsWith("courses/")) {
				if (!file.dir) {
					const fileContent = await file.async("string");
					try {
						// Parse the file content as JSON
						const jsonContent = JSON.parse(fileContent);
						fileMap.set(fileName, jsonContent);
					} catch (_err) {
						// Do nothing
					}
				}
			}
		})
	);

	// No Valid course sections
	if (fileMap.size === 0) {
		throw new InsightError("No valid course sections found in zip!");
	}

	return fileMap;
}

// Process rooms based on the room dataset specification
export function processRooms(fileMap: Map<string, any>): any[] {
	const rooms: any[] = [];
	fileMap.forEach((buildingRooms) => {
		for (const room of buildingRooms) {
			if (isValidRoom(room)) {
				rooms.push(room);
			}
		}
	});

	if (rooms.length === 0) {
		throw new InsightError("No valid rooms found in dataset!");
	}
	return rooms;
}

// Validate if a room contains all required fields with correct types
function isValidRoom(room: any): boolean {
	const requiredKeys = [
		{ key: "fullname", type: "string" },
		{ key: "shortname", type: "string" },
		{ key: "number", type: "string" },
		{ key: "name", type: "string" },
		{ key: "address", type: "string" },
		{ key: "lat", type: "number" },
		{ key: "lon", type: "number" },
		{ key: "seats", type: "number" },
		{ key: "type", type: "string" },
		{ key: "furniture", type: "string" },
		{ key: "href", type: "string" },
	];

	return requiredKeys.every(({ key, type }) => {
		const value = room[key];
		return typeof value === type && value !== null && value !== undefined;
	});
}

// Processes sections
export function processSections(fileMap: Map<string, any>): any[] {
	const sections: any[] = [];

	// Iterate through each file and extract valid sections
	fileMap.forEach((fileContent, _fileName) => {
		if (!fileContent.result || !Array.isArray(fileContent.result)) {
			return; //Invalid structure of json
		}

		for (const section of fileContent.result) {
			if (isValidSection(section)) {
				sections.push(section);
			}
		}
	});

	// no valid sections found
	if (sections.length === 0) {
		throw new InsightError("No valid sections found in dataset!");
	}
	return sections;
}

// Checks if a section is valid
function isValidSection(section: any): boolean {
	const requiredKeys = ["Subject", "Course", "Avg", "Professor", "Title", "Pass", "Fail", "Audit", "id", "Year"];
	return requiredKeys.every((key) => Object.prototype.hasOwnProperty.call(section, key));
}

// Saves the processed dataset.
export async function saveDataset(id: string, sections: any[], kind: InsightDatasetKind): Promise<void> {
	const dataset = {
		id,
		kind,
		numRows: sections.length,
		sections,
	};

	try {
		await fs.ensureDir(DATASET_DIRECTORY);
	} catch (_err) {
		throw new InsightError(`Failed to create dataset directory`);
	}

	// file path where datasets will be saved
	const filePath = path.join(DATASET_DIRECTORY, `${id}.json`);

	try {
		// Write the dataset to a file
		await fs.writeJson(filePath, dataset, { encoding: "utf8", spaces: 2 });
	} catch (_err) {
		throw new InsightError(`Failed to save dataset to disk`);
	}
}

export async function getStoredDatasetIds(): Promise<string[]> {
	try {
		const dirExists = await fs.pathExists(DATASET_DIRECTORY);

		if (!dirExists) {
			return [];
		}

		const files = await fs.readdir(DATASET_DIRECTORY);

		if (files.length === 0) {
			return [];
		}

		return files.filter((file) => file.endsWith(".json")).map((file) => path.basename(file, ".json"));
	} catch (_err) {
		throw new InsightError("Failed to read dataset directory");
	}
}
