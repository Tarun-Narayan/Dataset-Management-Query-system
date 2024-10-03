import { InsightDatasetKind, InsightError, InsightResult } from "./IInsightFacade";
import path from "path";
import * as fs from "fs-extra";
import { Query, LogicComparison, SComparison, MComparison, Negation, Filter } from "./Query";

export async function getResults(query: Query): Promise<InsightResult[]> {
	const dataset = await getDataset(query);
	const sections = await getSections(query.WHERE, dataset);

	const result: InsightResult = {
		id: sections[0],
	};

	return [result];
}

async function getDataset(
	query: Query
): Promise<{ id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }> {
	try {
		const keyToParse = query.OPTIONS.COLUMNS[0];
		const id = keyToParse.split("_", 1)[0];

		const filePath = path.join("./data", `${id}.json`);
		return await fs.readJson(filePath);
	} catch {
		throw new InsightError("Referenced dataset ID had not been added yet");
	}
}

async function getSections(
	filter: Filter,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = [];

	if (Object.keys(filter).length === 0) {
		return dataset.sections;
	}

	if ("OR" in filter || "AND" in filter) {
		sections.push(await handleLogicComparison(filter as LogicComparison, dataset));
	}

	if ("GT" in filter || "LT" in filter || "EQ" in filter) {
		sections.push(await handleMComparison(filter as MComparison, dataset));
	}

	if ("IS" in filter) {
		sections.push(await handleSComparison(filter as SComparison, dataset));
	}

	if ("NOT" in filter) {
		sections.push(await handleNegation(filter as Negation, dataset));
	}

	return sections;
}

async function handleLogicComparison(
	filter: LogicComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = [];

	if ((filter as LogicComparison).AND) {
		sections.push(await handleAnd((filter as LogicComparison).AND, dataset));
	}
	if ((filter as LogicComparison).OR) {
		sections.push(await handleOr((filter as LogicComparison).OR, dataset));
	}
	return sections;
}

async function handleOr(
	filters: Filter[] | undefined,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections: any[] = [];

	if (filters) {
		await Promise.all(
			filters.map(async (filter) => {
				const toAdd = await getSections(filter, dataset);
				for (const section of toAdd) {
					if (!sections.includes(section)) {
						sections.push(section);
					}
				}
			})
		);
	}

	return sections;
}

async function handleAnd(
	filters: Filter[] | undefined,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections: any[] = [];
	const validSections: any[] = [];

	if (filters) {
		await Promise.all(
			filters.map(async (filter) => {
				if (validSections.length === 0) {
					const toAdd = await getSections(filter, dataset);
					for (const section of toAdd) {
						validSections.push(section);
					}
				} else {
					const toCheck = await getSections(filter, dataset);
					for (const section of toCheck) {
						if (validSections.includes(section)) {
							sections.push(section);
						}
					}
				}
			})
		);
	}

	return sections;
}

async function handleMComparison(
	filter: MComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	return [];
}

async function handleSComparison(
	filter: SComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections: any[] = [];
	const SKey = Object.keys(filter.IS)[0];
	const input = Object.values(filter.IS)[0];
	const id = SKey.split("_", 1)[0];
	const field = SKey.split("_", 1)[1];
	if (id !== dataset.id) {
		throw new InsightError("Query references multiple datasets");
	}

	await Promise.all(
		dataset.sections.map(async(section) => {
			if (await checkSection(section, field, input)) {
				sections.push(section);
			}
		}))

	return sections;
}

async function checkSection(section: any, field: string, input: string | number): Promise<Boolean> {
	let result: Promise<boolean>
	if (typeof input === "string") {

	}
	if (typeof input === "number") {

	}
	return result;
}

async function handleNegation(
	filter: Negation,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const invalidSections = await getSections(filter.NOT, dataset);
	const sections = [];

	for (const section of dataset.sections) {
		if (!invalidSections.includes(section)) {
			sections.push(section);
		}
	}

	return sections;
}
