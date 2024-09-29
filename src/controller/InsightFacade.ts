import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightResult,
	InsightError,
	ResultTooLargeError,
} from "./IInsightFacade";
import { validateDataset, parseZipFile, processSections, saveDataset, getStoredDatasetIds } from "./ZipDecoder";
import { Query, validateQuery, getResults } from "./Query";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		try {
			validateDataset(id, kind);
			const idsList = await getStoredDatasetIds();
			if (idsList.includes(id)) {
				throw new InsightError(`Dataset with id ${id} already exists.`);
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
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::removeDataset() is unimplemented! - id=${id};`);
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
						return result;
					} catch (err) {
						return Promise.reject(err as ResultTooLargeError);
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
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::listDatasets is unimplemented!`);
	}
}
