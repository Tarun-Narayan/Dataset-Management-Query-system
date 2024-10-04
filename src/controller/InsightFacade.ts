import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightResult,
	InsightError,
	ResultTooLargeError,
	NotFoundError,
} from "./IInsightFacade";
import { validateDataset, parseZipFile, processSections, saveDataset, getStoredDatasetIds } from "./ZipDecoder";
import { Query, validateQuery } from "./Query";
import { getResults } from "./GetResults";
import path from "node:path";
import * as fs from "fs-extra";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
// const datasets = new Map<string, { id: string; kind: InsightDatasetKind }>();

export default class InsightFacade implements IInsightFacade {
	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		try {
			validateDataset(id, kind);
			const idsList = await getStoredDatasetIds();
			if (idsList.includes(id)) {
				return Promise.reject(new InsightError(`Dataset with id ${id} already exists.`));
			}

			const fileMap = await parseZipFile(content);
			const sections = processSections(fileMap);
			await saveDataset(id, sections, kind);

			// Return the list of all dataset IDs on disk
			const updatedIds = await getStoredDatasetIds();
			return Promise.resolve(updatedIds);
		} catch (err) {
			// _err used everywhere to avoid ESLint no-unused-vars error
			return Promise.reject(err);
		}
	}

	public async removeDataset(id: string): Promise<string> {
		try {
			// Validate dataset ID (no underscores and not empty/whitespace)
			if (id.trim().length === 0 || id.includes("_")) {
				return Promise.reject(new InsightError("Invalid ID: ID is empty, only whitespace, or contains an underscore."));
			}

			// List of stored dataset IDs
			const storedDatasetIds = await getStoredDatasetIds();

			// Check if dataset exists
			if (!storedDatasetIds.includes(id)) {
				return Promise.reject(new NotFoundError(`Dataset with id ${id} not found.`));
			}

			// File path for the dataset
			const filePath = path.join("./data", `${id}.json`);

			// Check if the file exists on disk
			const fileExists = await fs.pathExists(filePath);
			if (!fileExists) {
				return Promise.reject(new NotFoundError(`Dataset with id ${id} not found on disk.`));
			}

			// Remove the file from disk
			await fs.remove(filePath);
			return Promise.resolve(id);
		} catch (err) {
			return Promise.reject(err);
		}
	}

	public async performQuery(query: unknown): Promise<InsightResult[]> {
		if (typeof query === "object") {
			let result: InsightResult[];
			let valid: boolean;
			try {
				valid = await validateQuery(query as Query);
				if (valid) {
					try {
						result = await getResults(query as Query);
						return Promise.resolve(result);
					} catch (err) {
						return Promise.reject(err);
					}
				}
				return Promise.reject(new InsightError("Invalid query syntax"));
			} catch (err) {
				return Promise.reject(err as InsightError);
			}
		}
		return Promise.reject(new InsightError("Query not an object"));
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		try {
			// Stored dataset IDs
			const datasetIds = await getStoredDatasetIds();

			// Map dataset IDs to promises for reading the dataset files. Used Chatgpt for logic to avoid ESLint error
			// for const dataset = await fs.readJson(filePath); Created an ID->Promise mapping
			const datasetPromises = datasetIds.map(async (id) => {
				const filePath = path.join("./data", `${id}.json`);

				try {
					const dataset = await fs.readJson(filePath);

					// Extract (id, kind, numRows)
					const { kind, numRows } = dataset;
					return { id, kind, numRows };
				} catch (_err) {
					throw new InsightError(`Failed to read dataset file: ${id}`);
				}
			});

			return await Promise.all(datasetPromises);
		} catch (_err) {
			throw new InsightError("Failed to list datasets");
		}
	}
}
