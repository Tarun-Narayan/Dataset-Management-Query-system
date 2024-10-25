import { InsightError } from "./IInsightFacade";

export interface Query extends Object {
	WHERE: Filter;
	OPTIONS: Options;
	TRANSFORMATIONS?: Transformations;
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
	ORDER?: string | Order;
}

export interface Transformations {
	GROUP: string[];
	APPLY: ApplyRule[];
}

export type ApplyRule = Record<string, Record<string, string>>;

export interface Order {
	dir: "UP" | "DOWN";
	keys: string[];
}

const applyKeys = new Set<string>;


export async function validateQuery(query: Query): Promise<boolean> {
	let result3 = true;
	if (query.WHERE === undefined || query.WHERE === null) {
		throw new InsightError("Missing Body");
	}
	if (query.OPTIONS === undefined || query.OPTIONS === null) {
		throw new InsightError("Missing Options");
	}
	const keyNumber = 3;
	if (Object.keys(query).length === keyNumber) {
		if (query.TRANSFORMATIONS === undefined || query.TRANSFORMATIONS === null) {
			throw new InsightError("Missing Transformations");
		}
		result3 = await validateTransformations(query.TRANSFORMATIONS, query.OPTIONS.COLUMNS);
	}
	if (Object.keys(query).length > keyNumber) {
		throw new InsightError("Too many keys in Query");
	}

	const result1 = await validateBody(query.WHERE);
	const result2 = await validateOptions(query.OPTIONS);

	return result1 && result2 && result3;
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
	if (Object.keys(filter).length !== 1) {
		throw new InsightError("LogicComparison not formatted correctly");
	}
	let notEmpty = true;
	if (filter.AND && Array.isArray(filter.AND)) {
		for (const fil of filter.AND) {
			if (Object.keys(fil).length === 0) {
				notEmpty = false;
			}
		}
		const validResults = await Promise.all(filter.AND.map(validateBody));
		if (!(filter.AND.length > 0 && validResults.every(Boolean) && notEmpty)) {
			throw new InsightError("LogicComparison not formatted correctly");
		}
		return true;
	}
	if (filter.OR && Array.isArray(filter.OR)) {
		for (const fil of filter.OR) {
			if (Object.keys(fil).length === 0) {
				notEmpty = false;
			}
		}
		const validResults = await Promise.all(filter.OR.map(validateBody));
		if (!(filter.OR.length > 0 && validResults.every(Boolean) && notEmpty)) {
			throw new InsightError("LogicComparison not formatted correctly");
		}
		return true;
	}

	throw new InsightError("LogicComparison not formatted correctly");
}

async function validateMComparison(filter: MComparison): Promise<boolean> {
	if (Object.keys(filter).length !== 1) {
		throw new InsightError("MComparison not formatted correctly");
	}
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
		}
	}
	return true;
}
async function validateSComparison(filter: SComparison): Promise<boolean> {
	if (Object.keys(filter).length === 1 && filter.IS) {
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
	}
	return true;
}

async function validateNegation(filter: Negation): Promise<boolean> {
	if (Object.keys(filter).length === 1 && filter.NOT && Object.keys(filter.NOT).length !== 0) {
		return await validateBody(filter.NOT);
	}
	throw new InsightError("Negation not formatted correctly");
}

async function validateOptions(options: Options): Promise<boolean> {
	const maxKeys = 2;
	if (Object.keys(options).length === 0 || Object.keys(options).length > maxKeys) {
		throw new InsightError("Options not formatted correctly");
	}
	if (options.COLUMNS.length === 0 || !Array.isArray(options.COLUMNS)) {
		throw new InsightError("Option COLUMNS not formatted correctly");
	}
	// start chatGPT for help iterating with promises against linter
	const validationResults = await Promise.all(
		options.COLUMNS.map(async (key) => {
			return Promise.any([validateSKey(key), validateMKey(key), validateApplyKey(key)]).catch(() => false);
		})
	);
	// end chatGPT for help iterating with promises
	for (const result of validationResults) {
		if (!result) {
			throw new InsightError("Key in Options not MKey or SKey");
		}
	}
	if (options.ORDER) {
		return await validateSort(options.ORDER, options);
	}

	return true;
}

async function validateSort(order: string | Order, options: Options): Promise<boolean> {
	if (typeof order === "string") {
		if (!options.COLUMNS.includes(order)) {
			throw new InsightError("Order key must be in COLUMNS");
		}
	}
	if (typeof order === "object") {
		if (!(order.dir === "UP" || order.dir === "DOWN")) {
			throw new InsightError("Order direction is invalid");
		}
		if (order.keys.length === 0) {
			throw new InsightError("Order key list is empty");
		}
		for (const key of order.keys) {
			if (!options.COLUMNS.includes(key)) {
				throw new InsightError("Order key must be in COLUMNS");
			}
		}
	}
	return true;
}

async function validateMKey(mKey: string): Promise<boolean> {
	// start chatGPT help asserting string pattern
	const mKeyPattern = /^[^_]+_(avg|pass|fail|audit|year|lat|lon|seats)$/;
	if (mKeyPattern.test(mKey)) {
		// end chatGPT help asserting string pattern
		return true;
	}
	throw new InsightError("MKey not formatted correctly");
}

// start adapted from chatGPT
async function validateSKey(sKey: string): Promise<boolean> {
	const sKeyPattern =
		/^[^_]+_(dept|id|instructor|title|uuid|fullname|shortname|number|name|address|type|furniture|href)$/;
	if (sKeyPattern.test(sKey)) {
		return true;
	}
	throw new InsightError("SKey not formatted correctly");
}

async function validateInputString(string: string): Promise<boolean> {
	const stringPattern = /^\*?[^*]+\*?$/;
	if (stringPattern.test(string)) {
		return true;
	}
	throw new InsightError("InputString not formatted correctly");
}

async function validateTransformations(transform: Transformations, columns: string[]): Promise<boolean> {
	if (transform.GROUP.length === 0) {
		throw new InsightError("Group cannot be empty array");
	}
	for (const key of columns) {
		if (!applyKeys.has(key) && !columns.includes(key)) {
			throw new InsightError("COLUMNS keys must correspond to GROUP or Apply Keys");
		}
	}
	const validationGroup = await Promise.all(
		transform.GROUP.map(async (key) => {
			return Promise.any([validateSKey(key), validateMKey(key)]).catch(() => false);
		})
	);
	for (const result of validationGroup) {
		if (!result) {
			throw new InsightError("Key in GROUP is not MKey or SKey");
		}
	}

	const validationApply = await Promise.all(
		transform.APPLY.map(async (rule) => {
			return await validateApplyRule(rule);
		})
	);
	for (const result of validationApply) {
		if (!result) {
			throw new InsightError("Rule in APPLY is not formatted correctly");
		}
	}

	return true;
}

async function validateApplyRule(rule: ApplyRule): Promise<boolean> {
	if (Object.keys(rule).length !== 1) {
		throw new InsightError("Too many keys in ApplyRule");
	}
	const applyKey = Object.keys(rule)[0];
	if (!validateApplyKey(applyKey)) {
		throw new InsightError("ApplyKey not formatted correctly");
	}
	applyKeys.add(applyKey);
	const record = Object.values(rule)[0];

	return await validateApplyRecord(record);
}

function validateApplyKey(key: string): boolean {
	const applyKeyPattern = /^[^_]+$/;
	if (applyKeys.has(key)) {
		throw new InsightError("Apply Keys must be unique");
	}
	return applyKeyPattern.test(key);
}

async function validateApplyRecord(record: Record<string, string>): Promise<boolean> {
	if (Object.keys(record).length !== 1) {
		throw new InsightError("Incorrect number of keys in Apply Record");
	}
	const token = Object.keys(record)[0];
	if (!(token === "MAX" || token === "MIN" || token === "AVG" || token === "SUM" || token === "COUNT")) {
		throw new InsightError("Apply token is not formatted correctly");
	}
	const validateKey = await Promise.any([
		validateSKey(Object.values(record)[0]),
		validateMKey(Object.values(record)[0]),
	]).catch(() => false);
	if (!validateKey) {
		throw new InsightError("Apply rule key not MKey or SKey");
	}
	return true;
}
