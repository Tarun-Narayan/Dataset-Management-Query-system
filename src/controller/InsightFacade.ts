import {
	IInsightFacade,
	InsightDataset,
	InsightDatasetKind,
	InsightError,
	InsightResult,
	ResultTooLargeError,
} from "./IInsightFacade";
import { Query, validateQuery } from "./Query";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {
	public async addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
		// TODO: Remove this once you implement the methods!
		throw new Error(
			`InsightFacadeImpl::addDataset() is unimplemented! - id=${id}; content=${content?.length}; kind=${kind}`
		);
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
						result = await this.getResults(query as Query);
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

	private async getResults(query: Query): Promise<InsightResult[]> {

		return [];
	}

	public async listDatasets(): Promise<InsightDataset[]> {
		// TODO: Remove this once you implement the methods!
		throw new Error(`InsightFacadeImpl::listDatasets is unimplemented!`);
	}
}
