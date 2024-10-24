import JSZip from "jszip";
import { InsightError } from "./IInsightFacade";
import * as parse5 from "parse5";
import http from "http";

// Function to get the root folder name from the zip file
function getRootFolderName(zipContent: JSZip): string {
	const firstFileName = Object.keys(zipContent.files)[0];
	return firstFileName.split("/")[0];
}

// Function to parse rooms dataset
export async function parseRoomsZipFile(zipContent: JSZip): Promise<Map<string, any>> {
	const fileMap = new Map<string, any>();
	const rootFolder = getRootFolderName(zipContent);
	const indexFile = zipContent.files["campus/index.htm"];

	if (!zipContent.files["campus/index.htm"]) {
		throw new InsightError("Missing index.htm file in the zip!");
	}

	const indexContent = await indexFile.async("string");
	const buildings = parseBuildingDataFromHtml(indexContent);

	if (buildings.length === 0) {
		throw new InsightError("No valid buildings found in index.htm!");
	}

	const promises: Promise<void>[] = buildings.map(async (building) => {
		if (building.detailsLink) {
			const two = 2;
			//Accessing the building files
			const res = `${rootFolder}/${building.detailsLink.substring(two).trim()}`;
			const buildingFile = zipContent.files[res];

			if (buildingFile) {
				const buildingContent = await buildingFile.async("string");
				const rooms = await parseRoomDataFromHtml(buildingContent, building);

				//Doing this to maintain fileMap structure
				if (rooms.length > 0) {
					fileMap.set(building.buildingCode, rooms);
				}
			}
		}
	});

	await Promise.all(promises);

	if (fileMap.size === 0) {
		throw new InsightError("No valid rooms found in dataset!");
	}

	return fileMap;
}

// Parse room data from HTML, using the building information
async function parseRoomDataFromHtml(htmlContent: string, building: any): Promise<any[]> {
	const document = parse5.parse(htmlContent);
	const rooms: Promise<any | null>[] = [];
	//Finding Room table in the HTML file
	const roomTableNode = findElementByClassName(document, "views-table cols-5 table");

	if (roomTableNode) {
		const tbodyNode = roomTableNode.childNodes.find((node: any) => node.nodeName === "tbody");
		if (tbodyNode) {
			for (const row of tbodyNode.childNodes) {
				if (row.nodeName === "tr") {
					const roomDataPromise = extractRoomData(row, building);
					rooms.push(roomDataPromise);
				}
			}
		}
	}
	return await Promise.all(rooms);
}

// Fetch geolocation data
async function fetchGeolocation(address: string): Promise<{ lat?: number; lon?: number; error?: string }> {
	const encodedAddress = encodeURIComponent(address);
	const url = `http://cs310.students.cs.ubc.ca:11316/api/v1/project_team222/${encodedAddress}`;

	return new Promise((resolve, reject) => {
		http
			.get(url, (resp) => {
				let data = "";
				//ChatGPT help to understand how data is received
				resp.on("data", (chunk) => {
					data += chunk;
				});
				resp.on("end", () => {
					try {
						const geoResponse = JSON.parse(data);
						resolve(geoResponse);
					} catch (error) {
						reject(error);
					}
				});
			})
			.on("error", (err) => {
				reject(err);
			});
	});
}

// Extract relevant room data for each room
async function extractRoomData(row: any, building: any): Promise<any | null> {
	const roomNumberNode = findElementByClassName(row, "views-field-field-room-number");
	const capacityNode = findElementByClassName(row, "views-field-field-room-capacity");
	const furnitureTypeNode = findElementByClassName(row, "views-field-field-room-furniture");
	const roomTypeNode = findElementByClassName(row, "views-field-field-room-type");
	const roomInfoNode = findElementByClassName(row, "views-field views-field-nothing");

	const capacity = capacityNode?.childNodes[0]?.value.trim()
		? parseFloat(capacityNode.childNodes[0].value.trim())
		: null;
	const furnitureType = furnitureTypeNode?.childNodes[0]?.value.trim() || null;
	const roomType = roomTypeNode?.childNodes[0]?.value.trim() || null;
	const roomInfoLink = extractMoreInfoLink(roomInfoNode);
	const name = building.buildingCode + "_" + extractRoomNumber(roomNumberNode);
	try {
		// geolocation data
		const geoResponse = await fetchGeolocation(building.buildingAddress);

		if (geoResponse.error) {
			return geoResponse.error;
		}

		return {
			fullname: building.buildingName, // Full building name
			shortname: building.buildingCode, // Short building name
			number: extractRoomNumber(roomNumberNode), // Room number
			name: name, // Room ID (shortname + room number)
			address: building.buildingAddress, // Building address
			lat: geoResponse.lat, // Latitude
			lon: geoResponse.lon, // Longitude
			seats: capacity, // Room capacity (number of seats)
			type: roomType, // Room type
			furniture: furnitureType, // Room furniture
			href: roomInfoLink, // Full details link
		};
	} catch (error) {
		return Promise.reject(error);
	}
}

// Extract Room number from <a> tag
function extractRoomNumber(roomNumberNode: any): string | null {
	const anchorNode = roomNumberNode?.childNodes.find((child: any) => child.nodeName === "a");
	return anchorNode?.childNodes[0]?.value.trim() || null;
}

// Helper function to find elements by class name
export function findElementByClassName(node: any, className: string): any {
	if (node.attrs) {
		for (const attr of node.attrs) {
			if (attr.name === "class" && attr.value.includes(className)) {
				return node;
			}
		}
	}
	if (node.childNodes) {
		for (const child of node.childNodes) {
			const found = findElementByClassName(child, className);
			if (found) {
				return found;
			}
		}
	}
	return null;
}

// Extract the table containing building data
function getTableNode(document: any): any | null {
	return findElementByClassName(document, "views-table cols-5 table");
}

// Extract the building link (detailsLink)
function extractDetailsLink(buildingNameNode: any): string | null {
	const anchorNode = buildingNameNode?.childNodes.find((child: any) => child.nodeName === "a");
	return anchorNode?.attrs?.find((attr: any) => attr.name === "href")?.value || null;
}
// Extract the More Info link
function extractMoreInfoLink(RoomInfoNode: any): string | null {
	const anchorNode = RoomInfoNode?.childNodes.find((child: any) => child.nodeName === "a");
	return anchorNode?.attrs?.find((attr: any) => attr.name === "href")?.value || null;
}
// Extract Building Name from <a> tag
function extractBuildingName(buildingNameNode: any): string | null {
	const anchorNode = buildingNameNode?.childNodes.find((child: any) => child.nodeName === "a");
	return anchorNode?.childNodes[0]?.value.trim() || null;
}

// Extract relevant building data for each building
function extractBuildingData(row: any): any | null {
	const buildingCodeNode = findElementByClassName(row, "views-field-field-building-code");
	const buildingNameNode = findElementByClassName(row, "views-field-title");
	const buildingAddressNode = findElementByClassName(row, "views-field-field-building-address");

	const buildingCode = buildingCodeNode?.childNodes[0]?.value.trim() || null;
	const buildingName = extractBuildingName(buildingNameNode);
	const buildingAddress = buildingAddressNode?.childNodes[0]?.value.trim() || null;
	const detailsLink = extractDetailsLink(buildingNameNode);

	// We may need to change this to && depending on the requirements
	return buildingCode || buildingName || buildingAddress
		? {
				buildingCode,
				buildingName,
				buildingAddress,
				detailsLink,
		  }
		: null;
}

// Parse building data from HTML
export function parseBuildingDataFromHtml(htmlContent: string): any[] {
	const document = parse5.parse(htmlContent);
	const buildings: any[] = [];

	const tableNode = getTableNode(document);

	if (tableNode) {
		const tbodyNode = tableNode.childNodes.find((node: any) => node.nodeName === "tbody");
		if (tbodyNode) {
			tbodyNode.childNodes.forEach((row: any) => {
				if (row.nodeName === "tr") {
					const buildingData = extractBuildingData(row);
					if (buildingData) {
						buildings.push(buildingData);
					}
				}
			});
		}
	}

	return buildings;
}
