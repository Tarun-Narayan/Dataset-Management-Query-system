import { ApplyRule, Transformations } from "./Query";
import {InsightResult} from "./IInsightFacade";
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

export async function handleTransformations(
	transforms: Transformations,
	objects: InsightResult[],
	sections: any[]
): Promise<InsightResult[]> {
	const groupedResults = handleGroup(transforms.GROUP, objects);
	return await handleApply(transforms.APPLY, groupedResults, transforms.GROUP, sections);
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
	sections: any[]
): Promise<InsightResult[]> {
	const result = new Array<InsightResult>();
	for (const rule of apply) {
		for (const group of Object.values(groups)) {
			const toAdd: InsightResult = {};
			for (const field of fields) {
				toAdd[field] = group[0][field];
			}
			toAdd[Object.keys(rule)[0]] = handleRule(rule, group, sections);
			result.push(toAdd);
		}
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
	let total = new Decimal(0);
	let numRows = 0;
	for (const insight of group) {
		const section = getSection(sections, insight);
		const mappedKey = mapping[key.split("_")[1]];
		const decimal = new Decimal(section[mappedKey] as number);
		total = total.add(decimal);
		numRows++;
	}
	const avg = total.toNumber() / numRows;
	return Number(avg.toFixed(rounding));
}
function handleMax(group: InsightResult[], key: string, sections: any[]): number {
	let currentMax = group[0][key] as number;
	for (const insight of group) {
		if (insight[key] > currentMax) {
			currentMax = insight[key] as number;
		}
	}
	return currentMax;
}
function handleMin(group: InsightResult[], key: string, sections: any[]): number {
	let currentMin = group[0][key] as number;
	for (const insight of group) {
		if (insight[key] < currentMin) {
			currentMin = insight[key] as number;
		}
	}
	return currentMin;
}
function handleSum(group: InsightResult[], key: string, sections: any[]): number {
	let total = 0;
	for (const insight of group) {
		total = total + (insight[key] as number);
	}
	return Number(total.toFixed(rounding));
}
function handleCount(group: InsightResult[], key: string, sections: any[]): number {
	const counted = new Set<string | number>();
	let total = 0;
	for (const insight of group) {
		if (!counted.has(insight[key])) {
			total++;
			counted.add(insight[key]);
		}
	}
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
		return true;
	})
}
