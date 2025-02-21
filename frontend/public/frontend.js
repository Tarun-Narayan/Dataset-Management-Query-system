const BASE_URL = "http://localhost:4321";

document.getElementById("add-button").addEventListener("click", handleAddClick);

function handleAddClick() {
	window.open("../addMenu.html", "", "popup=true, width=500, height=150");
}
// Removed Datasets Button
document.getElementById("removed-list-button").addEventListener("click", handleRemovedListClick);

function handleRemovedListClick() {
	window.location.href = "../removedList.html";
}


// Populate Existing Datasets
async function loadDatasets() {
	try {
		const response = await fetch(`${BASE_URL}/datasets`, {
			method: "GET",
		});
		if (!response.ok) {
			throw new Error("Failed to fetch datasets");
		}
		const data = await response.json();
		const datasets = data.result;
		const datasetList = document.getElementById("dataset-list");
		datasetList.innerHTML = "";

		if (datasets.length === 0) {
			const noDataMessage = document.createElement("li");
			noDataMessage.textContent = "No datasets available.";
			datasetList.appendChild(noDataMessage);
		} else {
			datasets.forEach((dataset) => {
				const listItem = document.createElement("li");
				listItem.textContent = dataset.id;
				listItem.classList.add("dataset-item");
				listItem.addEventListener("click", () => handleDatasetClick(dataset.id));
				datasetList.appendChild(listItem);
			});
		}
	} catch (error) {
		console.error("Error loading datasets:", error);
		alert("Failed to load datasets. Try again." + `${error}`);
	}
}
window.loadDatasets = loadDatasets;


// Handle Dataset Click
function handleDatasetClick(datasetId) {
	const actionsDiv = document.getElementById("dataset-actions");
	const selectedDataset = document.getElementById("selected-dataset");
	selectedDataset.textContent = datasetId;
	actionsDiv.style.display = "block";
	document.getElementById("remove-dataset-button").onclick = () => handleRemoveDataset(datasetId);
	document.getElementById("insight1-button").onclick = () => handleInsight1(datasetId);
	// Bar Chart for Sections with Average > User Selected Number
	document.getElementById("insight2-button").onclick = () => handleInsight2(datasetId);
	document.getElementById("insight3-button").onclick = () => handleInsight3(datasetId);
}

// Remove Dataset
async function handleRemoveDataset(datasetId) {
	try {
		const response = await fetch(`${BASE_URL}/dataset/${datasetId}`, {
			method: "DELETE",
		});
		if (!response.ok) {
			throw new Error("Failed to remove dataset");
		}
		alert(`Dataset "${datasetId}" removed successfully.`);
		document.getElementById("dataset-actions").style.display = "none";
		await loadDatasets();
	} catch (error) {
		console.error("Error removing dataset:", error);
		alert("Failed to remove dataset. Please try again.");
	}
}

// View Insight Pass/Fail/Audit Insights
async function handleInsight1(datasetId) {
	const query = {
		WHERE: {

		},
		OPTIONS: {
			COLUMNS: [
				"sumFail",
				"sumPass",
				"sumAudit"
			]
		},
		TRANSFORMATIONS: {
			GROUP: [
				`${datasetId}_year`
			],
			APPLY: [
				{
					sumFail: {
						SUM: `${datasetId}_fail`
					}
				},
				{
					sumPass: {
						SUM: `${datasetId}_pass`
					}
				},
				{
					sumAudit: {
						SUM: `${datasetId}_audit`
					}
				}
			]
		}
	};
	try {
		const response = await fetch(`${BASE_URL}/query`, {
			method: "POST",
			headers: {"Content-Type": "application/json"},
			body: JSON.stringify(query)
		})
		if (!response.ok) {
			alert(`Error querying dataset: ${await response.text()}`);
		} else {
			const data = await response.json();
			const queryResults = data.result;
			const fields = JSON.stringify(getPieChartData(queryResults));
			window.location.href = `../insight1.html?queryResult=${encodeURIComponent(fields)}`;
		}

	} catch (e) {
		console.error("Error querying dataset: " + e);
	}
}
function getPieChartData(results) {
	let passTotal = 0;
	let failTotal = 0;
	let auditTotal = 0;
	for (const result of results) {
		passTotal += result.sumPass;
		failTotal += result.sumFail;
		auditTotal += result.sumAudit;
	}
	return [passTotal, failTotal, auditTotal];
}

// Bar Chart for sections with average > selected number
async function handleInsight2(datasetId) {
	const selectedValue = prompt("Enter a threshold for average:");
	const threshold = parseFloat(selectedValue);

	if (isNaN(threshold)) {
		alert("Invalid input. Please enter a numeric value.");
		return;
	}

	const query = {
		WHERE: {
			GT: {
				[`${datasetId}_avg`]: threshold
			}
		},
		OPTIONS: {
			COLUMNS: [
				`${datasetId}_title`,
				"avgColumn"
			],
			ORDER: {
				dir: "DOWN",
				keys: ["avgColumn"]
			}
		},

		TRANSFORMATIONS: {
			GROUP: [
				`${datasetId}_title`
			],
			APPLY: [
				{
					avgColumn: {
						MAX: `${datasetId}_avg`
					}
				}
			]
		}
	};

	try {
		const response = await fetch(`${BASE_URL}/query`, {
			method: "POST",
			headers: { "Content-Type": "application/json" },
			body: JSON.stringify(query)
		});

		if (!response.ok) {
			alert(`Error querying dataset: ${await response.text()}`);
		} else {
			const data = await response.json();
			const queryResults = data.result;
			const fields = JSON.stringify(getBarChartData(queryResults, datasetId));
			window.location.href = `../insight2.html?queryResult2=${encodeURIComponent(fields)}&selectedValue=${encodeURIComponent(selectedValue)}`;
		}
	} catch (e) {
		console.error("Error querying dataset: " + e);
	}
}

function getBarChartData(results, datasetId) {
	let titles = [];
	let averages = [];
	for (const result of results) {
		titles.push(result[`${datasetId}_title`]);
		averages.push(result.avgColumn);
	}
	return [titles, averages];
}

//Line Chart for subject average over the years
async function handleInsight3(datasetId) {
	const selectedCourse = prompt("Enter the course Subject/department to view its average over the years:");

	if (!selectedCourse || selectedCourse.trim() === "") {
		alert("Invalid course title. Please try again.");
		return;
	}

	const query = {
		WHERE: {
			IS: {
				[`${datasetId}_dept`]: selectedCourse.trim()
			}
		},
		OPTIONS: {
			COLUMNS: [
				`${datasetId}_year`,
				"avgColumn"
			],
			ORDER: {
				dir: "UP",
				keys: [`${datasetId}_year`]
			}
		},
		TRANSFORMATIONS: {
			GROUP: [
				`${datasetId}_year`
			],
			APPLY: [
				{
					avgColumn: {
						AVG: `${datasetId}_avg`
					}
				}
			]
		}
	};

	try {
		const response = await fetch(`${BASE_URL}/query`, {
			method: "POST",
			headers: { "Content-Type": "application/json" },
			body: JSON.stringify(query)
		});

		if (!response.ok) {
			alert(`Error querying dataset: ${await response.text()}`);
		} else {
			const data = await response.json();
			const queryResults = data.result;
			const fields = JSON.stringify(getLineChartData(queryResults, datasetId));
			window.location.href = `../insight3.html?queryResult3=${encodeURIComponent(fields)}&selectedCourse=${encodeURIComponent(selectedCourse)}`;

		}
	} catch (e) {
		console.error("Error querying dataset: " + e);
	}
}

function getLineChartData(results, datasetId) {
	let years = [];
	let averages = [];
	for (const result of results) {
		years.push(result[`${datasetId}_year`]);
		averages.push(result.avgColumn);
	}
	return [years, averages];
}




// Load datasets
window.onload = async () => {
	await loadDatasets();
};


