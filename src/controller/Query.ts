import { InsightError } from "./IInsightFacade";

export interface Query extends Object {
	WHERE: Filter;
	OPTIONS: Options;
}
type Filter = LogicComparison | MComparison | SComparison | Negation | {};

interface LogicComparison {
	AND?: Filter[];
	OR?: Filter[];
}
interface MComparison {
	EQ?: Record<string, number>;
	GT?: Record<string, number>;
	LT?: Record<string, number>;
}
interface SComparison {
	IS: Record<string, string>;
}
interface Negation {
	NOT: Filter;
}
interface Options {
	COLUMNS: string[];
	ORDER?: string;
}

export async function validateQuery(query: Query): Promise<boolean> {
	if (query.WHERE === undefined || query.WHERE === null) {
		throw new InsightError("Missing Body");
	}
	if (query.OPTIONS === undefined || query.OPTIONS === null) {
		throw new InsightError("Missing Options");
	}

	const result1 = await validateBody(query.WHERE);
	const result2 = await validateOptions(query.OPTIONS);

	return result1 && result2;
}

async function validateBody(filter: object): Promise<boolean> {
	if (Object.keys(filter).length === 0) {
		return true;
	}

	if ("AND" in filter || "OR" in filter) {
		return validateLogicComparison(filter as LogicComparison);
	}
	if ("LT" in filter || "GT" in filter || "EQ" in filter) {
		return validateMComparison(filter as MComparison);
	}
	if ("IS" in filter) {
		return validateSComparison(filter as SComparison);
	}
	if ("NOT" in filter) {
		return validateNegation(filter as Negation);
	}

	throw new InsightError("Invalid filter key");
}

async function validateLogicComparison(filter: LogicComparison): Promise<boolean> {
	if (filter.AND) {
		return Array.isArray(filter.AND) && filter.AND.every(validateBody);
	}
	if (filter.OR) {
		return Array.isArray(filter.OR) && filter.OR.every(validateBody);
	}

	throw new InsightError("LogicComparison not formatted correctly");
}

async function validateMComparison(filter: MComparison): Promise<boolean> {
	if (filter.EQ) {
		return (
			Object.keys(filter.EQ).length === 1 &&
			(await validateMKey(Object.keys(filter.EQ)[0])) &&
			typeof Object.values(filter.EQ)[0] === "number"
		);
	}
	if (filter.LT) {
		return (
			Object.keys(filter.LT).length === 1 &&
			(await validateMKey(Object.keys(filter.LT)[0])) &&
			typeof Object.values(filter.LT)[0] === "number"
		);
	}
	if (filter.GT) {
		return (
			Object.keys(filter.GT).length === 1 &&
			(await validateMKey(Object.keys(filter.GT)[0])) &&
			typeof Object.values(filter.GT)[0] === "number"
		);
	}

	throw new InsightError("MComparison not formatted correctly");
}
async function validateSComparison(filter: SComparison): Promise<boolean> {
	if (filter.IS) {
		return (
			Object.keys(filter.IS).length === 1 &&
			(await validateSKey(Object.keys(filter.IS)[0])) &&
			typeof Object.values(filter.IS)[0] === "string" &&
			validateInputString(Object.values(filter.IS)[0])
		);
	}
	throw new InsightError("SComparison not formatted correctly");
}

async function validateNegation(filter: Negation): Promise<boolean> {
	if (filter.NOT) {
		return validateBody(filter);
	}
	throw new InsightError("Negation not formatted correctly");
}

async function validateOptions(options: Options): Promise<boolean> {
	if (options.COLUMNS.length === 0 || !Array.isArray(options.COLUMNS)) {
		throw new InsightError("Option COLUMNS not formatted correctly");
	}
	// start chatGPT for help iterating with promises
	const validationResults = await Promise.all(
		options.COLUMNS.map(async (key) => {
			return Promise.any([validateSKey(key), validateMKey(key)]).catch(() => false);
		})
	);
	// end chatGPT for help iterating with promises
	for (const result of validationResults) {
		if (!result) {
			throw new InsightError("Key not MKey or SKey");
		}
	}

	if (options.ORDER) {
		let validOrder = false;
		for (const key of options.COLUMNS) {
			if (key === options.ORDER) {
				validOrder = true;
			}
		}
		return validOrder;
	}
	throw new InsightError("Options not formatted correctly");
}

async function validateMKey(mKey: string): Promise<boolean> {
	// start chatGPT help asserting string pattern
	const mKeyPattern = /^[^_]+_(avg|pass|fail|audit|year)$/;
	if (mKeyPattern.test(mKey)) {
		// end chatGPT help asserting string pattern
		return true;
	}
	throw new InsightError("MKey not formatted correctly");
}

// start adapted from chatGPT
async function validateSKey(sKey: string): Promise<boolean> {
	const sKeyPattern = /^[^_]+_(dept|id|instructor|title|uuid)$/;
	if (sKeyPattern.test(sKey)) {
		return true;
	}
	throw new InsightError("SKey not formatted correctly");
}

async function validateInputString(string: string): Promise<boolean> {
	const stringPattern = /^(\*?[^*]+|[^*]+\*?)$/;
	if (stringPattern.test(string)) {
		return true;
	}
	throw new InsightError("InputString not formatted correctly");
}
// end adapted from chatGPT
