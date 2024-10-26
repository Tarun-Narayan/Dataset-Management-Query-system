import { ApplyRule, Transformations } from "./Query";
import { InsightResult } from "./IInsightFacade";
import Decimal from "decimal.js";

const rounding = 2;

export async function handleTransformations(
	transforms: Transformations,
	objects: InsightResult[]
): Promise<InsightResult[]> {
	const groupedResults = handleGroup(transforms.GROUP, objects);
	return await handleApply(transforms.APPLY, groupedResults, transforms.GROUP);

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
	fields: string[]
): Promise<InsightResult[]> {
	const result = new Array<InsightResult>();
	for (const rule of apply) {
		for (const group of Object.values(groups)) {
			const toAdd: InsightResult = {};
			for (const field of fields) {
				toAdd[field] = group[0][field];
			}
			toAdd[Object.keys(rule)[0]] = handleRule(rule, group);
			result.push(toAdd);
		}
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
	let total = new Decimal(0);
	let numRows = 0;
	for (const insight of group) {
		const decimal = new Decimal(insight[key] as number);
		total = total.add(decimal);
		numRows++;
	}
	const avg = total.toNumber() / numRows;
	return Number(avg.toFixed(rounding));
}
function handleMax(group: InsightResult[], key: string): number {
	let currentMax = group[0][key] as number;
	for (const insight of group) {
		if (insight[key] > currentMax) {
			currentMax = insight[key] as number;
		}
	}
	return currentMax;
}
function handleMin(group: InsightResult[], key: string): number {
	let currentMin = group[0][key] as number;
	for (const insight of group) {
		if (insight[key] < currentMin) {
			currentMin = insight[key] as number;
		}
	}
	return currentMin;
}
function handleSum(group: InsightResult[], key: string): number {
	let total = 0;
	for (const insight of group) {
		total = total + (insight[key] as number);
	}
	return Number(total.toFixed(rounding));
}
function handleCount(group: InsightResult[], key: string): number {
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
