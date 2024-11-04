import { ApplyRule, Transformations, validateMKey, validateSKey } from "./Query";
import { InsightDatasetKind, InsightError, InsightResult } from "./IInsightFacade";
import Decimal from "decimal.js";
import { resultsMap } from "./GetResultObject";

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
const overallYear = 1900;

export async function handleTransformations(transforms: Transformations, columns: string[]): Promise<InsightResult[]> {
	const groupedResults = handleGroup(transforms.GROUP);
	addedSections.clear();
	return await handleApply(transforms.APPLY, groupedResults, transforms.GROUP, columns);
}

function handleGroup(group: string[]): Record<string, InsightResult[]> {
	const result: Record<string, InsightResult[]> = {};
	const groupNames = new Set<string>();
	const insights: IterableIterator<InsightResult> = resultsMap.keys();
	for (const object of insights) {
		const section = resultsMap.get(object);
		let groupName = "";
		for (const key of group) {
			const mappedKey = mapping[key.split("_")[1]];
			if (mappedKey === "Year" && section.Section === "overall") {
				section.Year = 1900;
			}
			const value = String(section[mappedKey]);
			groupName = groupName.concat(value, ", ");
		}
		const toErase = -2;
		groupName = groupName.slice(0, toErase);
		groupName = groupName.concat(" group");
		if (!groupNames.has(groupName)) {
			groupNames.add(groupName);
			result[groupName] = new Array<InsightResult>();
		}
		result[groupName].push(object);
	}
	return result;
}

async function handleApply(
	apply: ApplyRule[],
	groups: Record<string, InsightResult[]>,
	fields: string[],
	columns: string[]
): Promise<InsightResult[]> {
	const result = new Array<InsightResult>();
	for (const group of Object.values(groups)) {
		const toAdd: InsightResult = {};
		for (const field of fields) {
			const key = field.split("_")[1];
			if (columns.includes(field)) {
				toAdd[field] = group[0][key];
			}
		}
		for (const rule of apply) {
			if (columns.includes(Object.keys(rule)[0])) {
				toAdd[Object.keys(rule)[0]] = handleRule(rule, group);
			}
		}
		result.push(toAdd);
	}
	return result;
}

function handleRule(rule: ApplyRule, group: InsightResult[]): number {
	const token = Object.keys(Object.values(rule)[0])[0];
	const key = Object.values(Object.values(rule)[0])[0];
	let result = 0;
	if (token === "AVG") {
		result = handleAverage(group, key);
	}
	if (token === "MAX") {
		result = handleMax(group, key);
	}
	if (token === "MIN") {
		result = handleMin(group, key);
	}
	if (token === "SUM") {
		result = handleSum(group, key);
	}
	if (token === "COUNT") {
		result = handleCount(group, key);
	}
	return result;
}

function handleAverage(group: InsightResult[], key: string): number {
	const mappedKey = mapping[key.split("_")[1]];
	let total = new Decimal(0);
	let numRows = 0;
	for (const insight of group) {
		const section = resultsMap.get(insight);
		let value = section[mappedKey];
		if (mappedKey === "Year") {
			if (section.Section === "overall") {
				value = overallYear;
			} else {
				value = Number(value);
			}
		}
		const decimal = new Decimal(value as number);
		total = total.add(decimal);
		numRows++;
	}
	const avg = total.toNumber() / numRows;
	addedSections.clear();
	return Number(avg.toFixed(rounding));
}
function handleMax(group: InsightResult[], key: string): number {
	const mappedKey = mapping[key.split("_")[1]];
	const firstSection = resultsMap.get(group[0]);
	let currentMax = firstSection[mappedKey] as number;
	let first = true;
	for (const insight of group) {
		if (!first) {
			const section = resultsMap.get(insight);
			let value = section[mappedKey];
			if (mappedKey === "Year") {
				if (section.Section === "overall") {
					value = overallYear;
				} else {
					value = Number(value);
				}
			}
			if (value > currentMax) {
				currentMax = value as number;
			}
		} else {
			first = false;
		}
	}
	addedSections.clear();
	return currentMax;
}
function handleMin(group: InsightResult[], key: string): number {
	const mappedKey = mapping[key.split("_")[1]];
	const firstSection = resultsMap.get(group[0]);
	let currentMin = firstSection[mappedKey] as number;
	let first = true;
	for (const insight of group) {
		if (!first) {
			const section = resultsMap.get(insight);
			let value = section[mappedKey];
			if (mappedKey === "Year") {
				if (section.Section === "overall") {
					value = overallYear;
				} else {
					value = Number(value);
				}
			}
			if (value < currentMin) {
				currentMin = value as number;
			}
		} else {
			first = false;
		}
	}
	addedSections.clear();
	return currentMin;
}
function handleSum(group: InsightResult[], key: string): number {
	const mappedKey = mapping[key.split("_")[1]];
	let total = 0;
	for (const insight of group) {
		const section = resultsMap.get(insight);
		let value = section[mappedKey];
		if (mappedKey === "Year") {
			if (section.Section === "overall") {
				value = overallYear;
			} else {
				value = Number(value);
			}
		}
		total = total + (value as number);
	}
	addedSections.clear();
	return Number(total.toFixed(rounding));
}
function handleCount(group: InsightResult[], key: string): number {
	const mappedKey = mapping[key.split("_")[1]];
	const counted = new Set<string | number>();
	let total = 0;
	for (const insight of group) {
		const section = resultsMap.get(insight);
		let value = section[mappedKey];
		if (mappedKey === "Year") {
			if (section.Section === "overall") {
				value = overallYear;
			} else {
				value = Number(value);
			}
		}
		if (mappedKey === "id") {
			value = String(value);
		}
		if (!counted.has(value)) {
			total++;
			counted.add(value);
		}
	}
	addedSections.clear();
	return total;
}
/* Caused timeout???
function getSection(sections: any[], insight: InsightResult): any {
	return sections.find((element) => {
		for (const entry of Object.entries(insight)) {
			const updatedKey = mapping[entry[0].split("_")[1]];
			let value = element[updatedKey];
			if (updatedKey === "Year") {
				if (element.Section === "overall") {
					value = overallYear;
				} else {
					value = Number(value);
				}
			}
			if (updatedKey === "id") {
				value = String(value);
			}
			if (value !== entry[1]) {
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

 */

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
