import { InsightError } from "./IInsightFacade";

export interface Query extends Object {
	WHERE: Filter;
	OPTIONS: Options;
}
export type Filter = LogicComparison | MComparison | SComparison | Negation | {};

export interface LogicComparison {
	AND?: Filter[];
	OR?: Filter[];
}
export interface MComparison {
	EQ?: Record<string, number>;
	GT?: Record<string, number>;
	LT?: Record<string, number>;
}
export interface SComparison {
	IS: Record<string, string>;
}
export interface Negation {
	NOT: Filter;
}
export interface Options {
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
	let notEmpty = true;
	if (filter.AND) {
		for (const fil of filter.AND) {
			if (Object.keys(fil).length === 0) {
				notEmpty = false;
			}
		}
		const validResults = await Promise.all(filter.AND.map(validateBody));
		if (!(Array.isArray(filter.AND) && filter.AND.length > 0 && validResults.every(Boolean) && notEmpty)) {
			throw new InsightError("LogicComparison not formatted correctly");
		}
		return true;
	}
	if (filter.OR) {
		for (const fil of filter.OR) {
			if (Object.keys(fil).length === 0) {
				notEmpty = false;
			}
		}
		const validResults = await Promise.all(filter.OR.map(validateBody));
		if (!(Array.isArray(filter.OR) && filter.OR.length > 0 && validResults.every(Boolean) && notEmpty)) {
			throw new InsightError("LogicComparison not formatted correctly");
		}
		return true;
	}

	throw new InsightError("LogicComparison not formatted correctly");
}

async function validateMComparison(filter: MComparison): Promise<boolean> {
	const key = Object.keys(filter)[0];
	if (key === "EQ" || key === "LT" || key === "GT") {
		const record = filter[key];
		if (record) {
			if (
				!(
					Object.keys(record).length === 1 &&
					(await validateMKey(Object.keys(record)[0])) &&
					typeof Object.values(record)[0] === "number"
				)
			) {
				throw new InsightError("MComparison not formatted correctly");
			}
			return true;
		}
	}

	throw new InsightError("MComparison not formatted correctly");
}
async function validateSComparison(filter: SComparison): Promise<boolean> {
	if (filter.IS) {
		if (
			!(
				Object.keys(filter.IS).length === 1 &&
				(await validateSKey(Object.keys(filter.IS)[0])) &&
				typeof Object.values(filter.IS)[0] === "string" &&
				(await validateInputString(Object.values(filter.IS)[0]))
			)
		) {
			throw new InsightError("SComparison not formatted correctly");
		}
		return true;
	}
	throw new InsightError("SComparison not formatted correctly");
}

async function validateNegation(filter: Negation): Promise<boolean> {
	if (filter.NOT) {
		if (!((await validateBody(filter.NOT)) && Object.keys(filter.NOT).length !== 0)) {
			throw new InsightError("Negation not formatted correctly");
		}
		return true;
	}
	throw new InsightError("Negation not formatted correctly");
}

async function validateOptions(options: Options): Promise<boolean> {
	if (Object.keys(options).length === 0) {
		throw new InsightError("Options not formatted correctly");
	}
	if (options.COLUMNS.length === 0 || !Array.isArray(options.COLUMNS)) {
		throw new InsightError("Option COLUMNS not formatted correctly");
	}
	// start chatGPT for help iterating with promises against linter
	const validationResults = await Promise.all(
		options.COLUMNS.map(async (key) => {
			return Promise.any([validateSKey(key), validateMKey(key)]).catch(() => false);
		})
	);
	// end chatGPT for help iterating with promises
	for (const result of validationResults) {
		if (!result) {
			throw new InsightError("Key in Options not MKey or SKey");
		}
	}

	if (typeof options.ORDER === "string") {
		if (!options.COLUMNS.includes(options.ORDER)) {
			throw new InsightError("Order key must be in COLUMNS");
		}
	}
	return true;
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
	const stringPattern = /^\*?[^*]*\*?$/;
	if (stringPattern.test(string)) {
		return true;
	}
	throw new InsightError("InputString not formatted correctly");
}
