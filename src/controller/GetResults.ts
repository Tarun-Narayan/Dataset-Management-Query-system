import { InsightDatasetKind, InsightError, InsightResult, ResultTooLargeError } from "./IInsightFacade";
import path from "path";
import * as fs from "fs-extra";
import { Filter, LogicComparison, MComparison, Negation, Order, Query, SComparison } from "./Query";
import { getResultObject, orderResults, orderResultsSingle, resultsMap } from "./GetResultObject";
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

async function getSections(
	filter: Filter,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<Set<any>> {
	const sections = new Set<any>();
	let toAdd: any[] = [];

	if (Object.keys(filter).length === 0) {
		for (const section of dataset.sections) {
			sections.add(section);
		}
	}

	if ("OR" in filter || "AND" in filter) {
		toAdd = await handleLogicComparison(filter as LogicComparison, dataset);
	}

	if ("GT" in filter || "LT" in filter || "EQ" in filter) {
		toAdd = await handleMComparison(filter as MComparison, dataset);
	}

	if ("IS" in filter) {
		toAdd = await handleSComparison(filter as SComparison, dataset);
	}

	if ("NOT" in filter) {
		toAdd = await handleNegation(filter as Negation, dataset);
	}
	for (const section of toAdd) {
		sections.add(section);
	}

	return sections;
}

async function handleLogicComparison(
	filter: LogicComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();

	if (filter.AND) {
		const toAdd = await handleAnd(filter.AND, dataset);
		for (const section of toAdd) {
			sections.add(section);
		}
	}
	if (filter.OR) {
		const toAdd = await handleOr(filter.OR, dataset);
		for (const section of toAdd) {
			sections.add(section);
		}
	}
	return Array.from(sections);
}

async function handleOr(
	filters: Filter[] | undefined,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();

	if (filters) {
		await Promise.all(
			filters.map(async (filter) => {
				const toAdd = await getSections(filter, dataset);
				for (const section of toAdd) {
					sections.add(section);
				}
			})
		);
	}

	return Array.from(sections);
}

async function handleAnd(
	filters: Filter[] | undefined,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();

	if (filters) {
		if (filters.length === 1) {
			return Array.from(await getSections(filters[0], dataset));
		}
		await getSections(filters[0], dataset).then(async (validSections) => {
			await Promise.all(
				filters.map(async (filter) => {
					if (filter !== filters[0]) {
						const toCheck = await getSections(filter, dataset);
						for (const section of toCheck) {
							if (validSections.has(section)) {
								sections.add(section);
							}
						}
					}
				})
			);
		});
	}
	return Array.from(sections);
}

async function handleMComparison(
	filter: MComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();
	let record: Record<string, number> = { "": 0 };
	let type = "GT";
	if (filter.EQ) {
		record = filter.EQ;
		type = "EQ";
	} else if (filter.LT) {
		record = filter.LT;
		type = "LT";
	} else if (filter.GT) {
		record = filter.GT;
	}
	const [MKey, input] = Object.entries(record)[0];
	if (MKey.split("_")[0] !== dataset.id) {
		throw new InsightError("Query references multiple datasets1");
	}

	await Promise.all(
		dataset.sections.map(async (section) => {
			if (dataset.kind === InsightDatasetKind.Sections) {
				if (await checkMSection(section, MKey.split("_")[1], input, type)) {
					sections.add(section);
				}
			} else {
				if (await checkMRoom(section, MKey.split("_")[1], input, type)) {
					sections.add(section);
				}
			}
		})
	);

	return Array.from(sections);
}

async function handleSComparison(
	filter: SComparison,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();
	const record = filter.IS;
	const [SKey, input] = Object.entries(record)[0];
	const id = SKey.split("_")[0];
	const field = SKey.split("_")[1];
	if (id !== dataset.id) {
		throw new InsightError("Query references multiple datasets2");
	}

	await Promise.all(
		dataset.sections.map(async (section) => {
			if (dataset.kind === InsightDatasetKind.Sections) {
				if (await checkSSection(section, field, input)) {
					sections.add(section);
				}
			} else {
				if (await checkSRoom(section, field, input)) {
					sections.add(section);
				}
			}
		})
	);

	return Array.from(sections);
}

async function checkSSection(section: any, field: string, input: string): Promise<Boolean> {
	let toCompare: string = section.Subject;
	if (field === "uuid") {
		toCompare = section.id;
	}
	if (field === "id") {
		toCompare = section.Course;
	}
	if (field === "title") {
		toCompare = section.Title;
	}
	if (field === "instructor") {
		toCompare = section.Professor;
	}
	return handleSCompare(input, toCompare);
}
async function checkSRoom(section: any, field: string, input: string): Promise<Boolean> {
	let toCompare: string = section.fullname;
	if (field === "shortname") {
		toCompare = section.shortname;
	}
	if (field === "number") {
		toCompare = section.number;
	}
	if (field === "name") {
		toCompare = section.name;
	}
	if (field === "address") {
		toCompare = section.address;
	}
	if (field === "type") {
		toCompare = section.type;
	}
	if (field === "furniture") {
		toCompare = section.furniture;
	}
	if (field === "href") {
		toCompare = section.href;
	}
	return handleSCompare(input, toCompare);
}

async function handleSCompare(input: string, toCompare: string): Promise<boolean> {
	if (input.startsWith("*") && input.endsWith("*")) {
		return toCompare.includes(input.slice(1, -1));
	}
	if (input.startsWith("*")) {
		return toCompare.endsWith(input.slice(1));
	}
	if (input.endsWith("*")) {
		return toCompare.startsWith(input.slice(0, -1));
	}
	return toCompare === input;
}

async function checkMSection(section: any, field: string, input: number, type: string): Promise<Boolean> {
	let toCompare: number = section.Audit;
	if (field === "avg") {
		toCompare = section.Avg;
	}
	if (field === "year") {
		toCompare = section.Year;
	}
	if (field === "pass") {
		toCompare = section.Pass;
	}
	if (field === "fail") {
		toCompare = section.Fail;
	}

	if (type === "GT") {
		return toCompare > input;
	}
	if (type === "EQ") {
		return toCompare === input;
	}
	return toCompare < input;
}
async function checkMRoom(section: any, field: string, input: number, type: string): Promise<Boolean> {
	let toCompare: number = section.lat;
	if (field === "lon") {
		toCompare = section.lon;
	}
	if (field === "seats") {
		toCompare = section.seats;
	}

	if (type === "GT") {
		return toCompare > input;
	}
	if (type === "EQ") {
		return toCompare === input;
	}
	return toCompare < input;
}
async function handleNegation(
	filter: Negation,
	dataset: { id: string; kind: InsightDatasetKind; numRows: number; sections: any[] }
): Promise<any[]> {
	const sections = new Set<any>();
	await getSections(filter.NOT, dataset).then((invalidSections) => {
		for (const section of dataset.sections) {
			if (!invalidSections.has(section)) {
				sections.add(section);
			}
		}
	});

	return Array.from(sections);
}
