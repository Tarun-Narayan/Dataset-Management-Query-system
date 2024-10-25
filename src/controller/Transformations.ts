import { Transformations } from "./Query";
import { InsightResult } from "./IInsightFacade";

export async function handleTransformations(
	transforms: Transformations,
	objects: InsightResult[]
): Promise<InsightResult[]> {
	const groupedResults = await handleGroup(transforms.GROUP, objects);

}

async function handleGroup(group: string[], objects: InsightResult[]):Promise<Record<string,InsightResult[]>> {
	const keys = new Set<string>;
	for (const object of objects) {
		let toAdd = "";
		for (const key of group) {
			const value = object[key].toString();
			toAdd = toAdd.concat(value, ", ");
		}
		toAdd = toAdd.slice(0, -2);
		toAdd = toAdd.concat(" group");
		keys.add(toAdd);

	}
}
