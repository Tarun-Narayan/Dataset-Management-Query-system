import { ApplyRule, Transformations, validateMKey, validateSKey } from "./Query";
import { InsightDatasetKind, InsightError, InsightResult } from "./IInsightFacade";
import Decimal from "decimal.js";

const rounding = 2;
const mapping: Record<string, string> = {
	uuid: "id",
	id: "Course",
	title: "Title",
	instructor: "Professor",
	dept: "Subject",
	year: "Year",
	avg: "Avg",
	pass: "Pass",
	fail: "Fail",
	audit: "Audit",
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
const addedSections = new Set<any>();

export async function handleTransformations(
	transforms: Transformations,
	objects: InsightResult[],
	sections: any[],
	columns: string[]
): Promise<InsightResult[]> {
	const groupedResults = handleGroup(transforms.GROUP, objects);
	return await handleApply(transforms.APPLY, groupedResults, transforms.GROUP, sections, columns);
}

function handleGroup(group: string[], objects: InsightResult[]): Record<string, InsightResult[]> {
	const result: Record<string, InsightResult[]> = {};
	const groupNames = new Set<string>();
	for (const object of objects) {
		let groupName = "";
		for (const key of group) {
			const value = object[key].toString();
			groupName = groupName.concat(value, ", ");
		}
		const toErase = -2;
		groupName = groupName.slice(0, toErase);
		groupName = groupName.concat(" group");
		if (!groupNames.has(groupName)) {
			groupNames.add(groupName);
			result[groupName] = new Array<InsightResult>();
		}
		const value = result[groupName];
		value.push(object);
		result[groupName] = value;
	}
	return result;
}

async function handleApply(
	apply: ApplyRule[],
	groups: Record<string, InsightResult[]>,
	fields: string[],
	sections: any[],
	columns: string[]
): Promise<InsightResult[]> {
	const result = new Array<InsightResult>();
	for (const group of Object.values(groups)) {
		const toAdd: InsightResult = {};
		for (const field of fields) {
			toAdd[field] = group[0][field];
		}
		for (const rule of apply) {
			if (columns.includes(Object.keys(rule)[0])) {
				toAdd[Object.keys(rule)[0]] = handleRule(rule, group, sections);
			}
		}
		result.push(toAdd);
	}
	return result;
}

function handleRule(rule: ApplyRule, group: InsightResult[], sections: any[]): number {
	const token = Object.keys(Object.values(rule)[0])[0];
	const key = Object.values(Object.values(rule)[0])[0];
	let result = 0;
	if (token === "AVG") {
		result = handleAverage(group, key, sections);
	}
	if (token === "MAX") {
		result = handleMax(group, key, sections);
	}
	if (token === "MIN") {
		result = handleMin(group, key, sections);
	}
	if (token === "SUM") {
		result = handleSum(group, key, sections);
	}
	if (token === "COUNT") {
		result = handleCount(group, key, sections);
	}
	return result;
}

function handleAverage(group: InsightResult[], key: string, sections: any[]): number {
	const mappedKey = mapping[key.split("_")[1]];
	let total = new Decimal(0);
	let numRows = 0;
	for (const insight of group) {
		const section = getSection(sections, insight);
		const decimal = new Decimal(section[mappedKey] as number);
		total = total.add(decimal);
		numRows++;
	}
	const avg = total.toNumber() / numRows;
	addedSections.clear();
	return Number(avg.toFixed(rounding));
}
function handleMax(group: InsightResult[], key: string, sections: any[]): number {
	const mappedKey = mapping[key.split("_")[1]];
	const firstSection = getSection(sections, group[0]);
	let currentMax = firstSection[mappedKey] as number;
	let first = true;
	for (const insight of group) {
		if (!first) {
			const section = getSection(sections, insight);
			if (section[mappedKey] > currentMax) {
				currentMax = section[mappedKey] as number;
			}
		} else {
			first = false;
		}
	}
	addedSections.clear();
	return currentMax;
}
function handleMin(group: InsightResult[], key: string, sections: any[]): number {
	const mappedKey = mapping[key.split("_")[1]];
	const firstSection = getSection(sections, group[0]);
	let currentMin = firstSection[mappedKey] as number;
	let first = true;
	for (const insight of group) {
		if (!first) {
			const section = getSection(sections, insight);
			if (section[mappedKey] < currentMin) {
				currentMin = section[mappedKey] as number;
			}
		} else {
			first = false;
		}
	}
	addedSections.clear();
	return currentMin;
}
function handleSum(group: InsightResult[], key: string, sections: any[]): number {
	const mappedKey = mapping[key.split("_")[1]];
	let total = 0;
	for (const insight of group) {
		const section = getSection(sections, insight);
		total = total + (section[mappedKey] as number);
	}
	addedSections.clear();
	return Number(total.toFixed(rounding));
}
function handleCount(group: InsightResult[], key: string, sections: any[]): number {
	const mappedKey = mapping[key.split("_")[1]];
	const counted = new Set<string | number>();
	let total = 0;
	for (const insight of group) {
		const section = getSection(sections, insight);
		if (!counted.has(section[mappedKey])) {
			total++;
			counted.add(section[mappedKey]);
		}
	}
	addedSections.clear();
	return total;
}

function getSection(sections: any[], insight: InsightResult): any {
	return sections.find((element) => {
		for (const entry of Object.entries(insight)) {
			const updatedKey = mapping[entry[0].split("_")[1]];
			if (element[updatedKey] !== entry[1]) {
				return false;
			}
		}
		if (!addedSections.has(element)) {
			addedSections.add(element);
			return true;
		}
		return false;
	});
}

export async function validateApplyRecord(record: Record<string, string>, kind: InsightDatasetKind): Promise<boolean> {
	if (Object.keys(record).length !== 1) {
		throw new InsightError("Incorrect number of keys in Apply Record");
	}
	const token = Object.keys(record)[0];
	if (!(token === "MAX" || token === "MIN" || token === "AVG" || token === "SUM" || token === "COUNT")) {
		throw new InsightError("Apply token is not formatted correctly");
	}
	if (token === "MAX" || token === "MIN" || token === "AVG" || token === "SUM") {
		if (!(await validateMKey(Object.values(record)[0], kind))) {
			throw new InsightError("Key must be numeric");
		}
	}
	const validateKey = await Promise.any([
		validateSKey(Object.values(record)[0], kind),
		validateMKey(Object.values(record)[0], kind),
	]).catch(() => false);
	if (!validateKey) {
		throw new InsightError("Apply rule key not MKey or SKey");
	}
	return true;
}
