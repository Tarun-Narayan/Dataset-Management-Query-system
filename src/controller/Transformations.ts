import { Transformations } from "./Query";
import { InsightResult } from "./IInsightFacade";

export async function handleTransformations(
	transforms: Transformations,
	objects: InsightResult[]
): Promise<InsightResult[]> {
	const groupedResults = handleGroup(transforms.GROUP, objects);
	return [];
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
		groupName = groupName.slice(0, -2);
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
