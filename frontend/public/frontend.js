document.getElementById("add-button").addEventListener("click", handleAddClick);

function handleAddClick() {
	window.open("../addMenu.html", "", "popup=true, width=500, height=150");
}


const BASE_URL = "http://localhost:4321";
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
window.loadDatasets = loadDatasets();


// Handle Dataset Click
function handleDatasetClick(datasetId) {
	const actionsDiv = document.getElementById("dataset-actions");
	const selectedDataset = document.getElementById("selected-dataset");
	selectedDataset.textContent = datasetId;
	actionsDiv.style.display = "block";
	document.getElementById("remove-dataset-button").onclick = () => handleRemoveDataset(datasetId);
	document.getElementById("insights-button").onclick = () => handleViewInsights(datasetId);
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

// View Insights
function handleViewInsights(datasetId) {
	alert(`Viewing insights for "${datasetId}"`);
}

// Load datasets
window.onload = async () => {
	await loadDatasets();
};


