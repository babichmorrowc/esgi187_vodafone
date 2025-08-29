# If this file is run directly, it will create a temporary environment and install the required packages
using Pkg
Pkg.activate(; temp = true)
Pkg.add(["CSV", "DataFrames", "Statistics", "CoordRefSystems"])

# Load packages
using CSV
using DataFrames
using Statistics
using CoordRefSystems

const DATA_PATH = joinpath(@__DIR__, "..", "data")
export DATA_PATH

const BNG = CoordRefSystems.get(EPSG{27700})

"""
    latlon2xy(lat, lon)

Convert latitude and longitude to British National Grid (BNG) coordinates.
"""
function latlon2xy(lat, lon)
    p = convert(BNG, LatLon(lat, lon))
    x = floor(Int, p.x.val)
    y = floor(Int, p.y.val)
    return (x, y)
end

"""
    combine_data_2024()

Load the Vodafone predictions and the OFCOM data, combine them, and save the result to a CSV file.
This function reads the Vodafone predictions for RSRP and PCI, the index to cell mapping,
and the cell to PCI mapping. It then reads the OFCOM data, combines it with the Vodafone predictions,
and saves the result to a CSV file.

RSRP values are filtered to be between -110 and -45 dBm, and only measurements
with a valid EARFCN of 6300 are considered.

The output CSV file contains the following columns:

- `x`: X coordinate in BNG
- `y`: Y coordinate in BNG
- `month_year`: Month and year of the measurement
- `hour_ref`: Hour reference of the measurement
- `voda_pci`: Predicted PCI from Vodafone
- `voda_rsrp`: Predicted RSRP from Vodafone
- `ofcom_pci_top1`: OFCOM PCI for the top 1 measurement
- `ofcom_rsrp_top1`: OFCOM RSRP for the top 1 measurement
- `ofcom_pci_top2`: OFCOM PCI for the top 2 measurement
- `ofcom_rsrp_top2`: OFCOM RSRP for the top 2 measurement
- `ofcom_pci_top3`: OFCOM PCI for the top 3 measurement
- `ofcom_rsrp_top3`: OFCOM RSRP for the top 3 measurement
- `ofcom_pci_top4`: OFCOM PCI for the top 4 measurement
- `ofcom_rsrp_top4`: OFCOM RSRP for the top 4 measurement
"""
function combine_data_2024()
    local pred_rsrp, pred_pci, x_origin, y_origin, bin_size
    # Load the matrix of Vodafone predictions
    open(joinpath(DATA_PATH, "Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz",
        "LTE  MIMO Terminal [No Loss] (DL) 800MHz.asc")) do f
        ncols = parse(Int, split(readline(f))[2])
        nrows = parse(Int, split(readline(f))[2])
        x_origin = parse(Float64, split(readline(f))[2])
        y_origin = parse(Float64, split(readline(f))[2])
        bin_size = parse(Float64, split(readline(f))[2])
        nodata_value = parse(Float64, split(readline(f))[2])
        @assert ncols == 6860
        @assert nrows == 12380
        pred_rsrp = Matrix{Union{Float64, Missing}}(undef, nrows, ncols)
        for i in 1:nrows
            line = readline(f)
            values = split(line)
            for j in 1:ncols
                value = parse(Float64, values[j])
                pred_rsrp[i, j] = value == nodata_value ? missing : value
            end
        end
    end
    # Load the matrix of Vodafone predicted PCIs
    open(joinpath(DATA_PATH, "Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz",
        "LTE  MIMO Terminal [No Loss] (DL) 800MHz.svr.asc")) do f
        ncols = parse(Int, split(readline(f))[2])
        nrows = parse(Int, split(readline(f))[2])
        x_origin = parse(Float64, split(readline(f))[2])
        y_origin = parse(Float64, split(readline(f))[2])
        bin_size = parse(Float64, split(readline(f))[2])
        nodata_value = parse(Int, split(readline(f))[2])
        @assert ncols == 6860
        @assert nrows == 12380
        pred_pci = Matrix{Union{Int, Missing}}(undef, nrows, ncols)
        for i in 1:nrows
            line = readline(f)
            values = split(line)
            for j in 1:ncols
                value = parse(Int, values[j])
                pred_pci[i, j] = value == nodata_value ? missing : value
            end
        end
    end
    # Load the index to cell mapping and the cell to PCI mapping
    index2cell_data = CSV.read(
        joinpath(DATA_PATH, "Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz",
            "LTE  MIMO Terminal [No Loss] (DL) 800MHz.svr.csv"), DataFrame; header = [
            "index", "name"])
    index2cell = Dict{Int, String}()
    for row in eachrow(index2cell_data)
        index2cell[row.index] = row.name
    end
    cell2pci_data = CSV.read(
        joinpath(DATA_PATH, "L8_Sept24 Sites TXs and Cells", "L8_Sept24 Cells.txt"),
        DataFrame; select = ["Transmitter", "Physical Cell ID"])
    cell2pci = Dict{String, Int}()
    for row in eachrow(cell2pci_data)
        # If the Physical Cell ID is missing (Friday problems...), we set it to -1
        if row.var"Physical Cell ID" !== missing
            cell2pci[row.Transmitter] = row.var"Physical Cell ID"
        else
            cell2pci[row.Transmitter] = -1
        end
    end
    # Convert the index values to PCI values
    for i in eachindex(pred_pci)
        if pred_pci[i] !== missing
            pred_pci[i] = cell2pci[index2cell[pred_pci[i]]]
        end
    end
    # Flip the matrices to match the expected coordinate system
    pred_rsrp = reverse(pred_rsrp, dims = 1)
    pred_pci = reverse(pred_pci, dims = 1)
    # Read the OFCOM data and combine it with the predictions - selecting only the relevant columns
    columns = Dict(["latitude" => Float64, "longitude" => Float64, "pci_top1_vf" => Int,
        "rsrp_top1_vf" => Float64, "earfcn_top1_vf" => Int, "pci_top2_vf" => Int,
        "rsrp_top2_vf" => Float64, "earfcn_top2_vf" => Int, "pci_top3_vf" => Int,
        "rsrp_top3_vf" => Float64, "earfcn_top3_vf" => Int, "pci_top4_vf" => Int,
        "rsrp_top4_vf" => Float64, "earfcn_top4_vf" => Int, "month_year" => String,
        "hour_ref" => String])
    # Create a data reader that works line by line to avoid loading the entire file into memory
    data = CSV.Rows(
        joinpath(DATA_PATH, "Tony_Vernon_2024 Ofcom 4G Measurement Data",
            "4g-lte-2024-mobile-signal-measurement-data.csv");
        select = collect(keys(columns)),
        types = columns
    )
    # Create an output CSV file with the combined data
    open(joinpath(DATA_PATH, "4g-lte-2024-mobile-signal-measurement-data-combined.csv"),
        "w") do f
        # Write the header
        println(f,
            "x,y,month_year,hour_ref,voda_pci,voda_rsrp,ofcom_pci_top1,ofcom_rsrp_top1,ofcom_pci_top2,ofcom_rsrp_top2,ofcom_pci_top3,ofcom_rsrp_top3,ofcom_pci_top4,ofcom_rsrp_top4")
        # Process each row of the data
        for row in data
            pt = latlon2xy(row.latitude, row.longitude)
            i_x = round(Int, (pt[1] - x_origin) ÷ bin_size + 1)
            i_y = round(Int, (pt[2] - y_origin) ÷ bin_size + 1)
            if i_x < 1 || i_y < 1 || i_x > size(pred_pci, 2) || i_y > size(pred_pci, 1)
                throw(ArgumentError("Point $pt is out of bounds for the prediction grid (lat=$(row.latitude), lon=$(row.longitude))."))
            end
            x = pt[1]
            y = pt[2]
            month_year = row.month_year
            hour_ref = row.hour_ref
            voda_pci = pred_pci[i_y, i_x] !== missing ? pred_pci[i_y, i_x] : ""
            voda_rsrp = pred_rsrp[i_y, i_x] !== missing ? pred_rsrp[i_y, i_x] : ""
            # OFCOM data - check if the values are valid
            top1 = (row.earfcn_top1_vf !== missing) &&
                   (row.earfcn_top1_vf == 6300) && (row.pci_top1_vf !== missing) &&
                   (row.rsrp_top1_vf !== missing) && (row.rsrp_top1_vf >= -110.0) &&
                   (row.rsrp_top1_vf <= -45.0)
            top2 = (row.earfcn_top2_vf !== missing) &&
                   (row.earfcn_top2_vf == 6300) && (row.pci_top2_vf !== missing) &&
                   (row.rsrp_top2_vf !== missing) && (row.rsrp_top2_vf >= -110.0) &&
                   (row.rsrp_top2_vf <= -45.0)
            top3 = (row.earfcn_top3_vf !== missing) &&
                   (row.earfcn_top3_vf == 6300) && (row.pci_top3_vf !== missing) &&
                   (row.rsrp_top3_vf !== missing) && (row.rsrp_top3_vf >= -110.0) &&
                   (row.rsrp_top3_vf <= -45.0)
            top4 = (row.earfcn_top4_vf !== missing) &&
                   (row.earfcn_top4_vf == 6300) && (row.pci_top4_vf !== missing) &&
                   (row.rsrp_top4_vf !== missing) && (row.rsrp_top4_vf >= -110.0) &&
                   (row.rsrp_top4_vf <= -45.0)
            # If none of the values are valid, skip the row
            if !top1 && !top2 && !top3 && !top4
                continue
            end
            ofcom_pci_top1 = top1 ? row.pci_top1_vf : ""
            ofcom_pci_top2 = top2 ? row.pci_top2_vf : ""
            ofcom_pci_top3 = top3 ? row.pci_top3_vf : ""
            ofcom_pci_top4 = top4 ? row.pci_top4_vf : ""
            ofcom_rsrp_top1 = top1 ? row.rsrp_top1_vf : ""
            ofcom_rsrp_top2 = top2 ? row.rsrp_top2_vf : ""
            ofcom_rsrp_top3 = top3 ? row.rsrp_top3_vf : ""
            ofcom_rsrp_top4 = top4 ? row.rsrp_top4_vf : ""
            # Write the combined data to the CSV file
            println(f,
                "$x,$y,$month_year,$hour_ref,$voda_pci,$voda_rsrp,$ofcom_pci_top1,$ofcom_rsrp_top1,$ofcom_pci_top2,$ofcom_rsrp_top2,$ofcom_pci_top3,$ofcom_rsrp_top3,$ofcom_pci_top4,$ofcom_rsrp_top4")
        end
    end
end

"""
    summarise_data_2024([data])

Summarise the supplied data (optional) or load the default data file and return a summary.

The summary includes the following fields:
- `tb`: A unique identifier for the data point (based on x and y coordinates)
- `x`: X coordinate in BNG
- `y`: Y coordinate in BNG
- `voda_pci`: Predicted PCI from Vodafone
- `voda_rsrp`: Predicted RSRP from Vodafone
- `ofcom_pci_mode`: The most common PCI from OFCOM measurements
- `ofcom_pci_mode_count`: The count of the most common PCI from OFCOM
- `ofcom_rsrp_match_mean`: Mean RSRP from OFCOM measurements that match the Vodafone PCI
- `ofcom_rsrp_match_median`: Median RSRP from OFCOM measurements that match the Vodafone PCI
- `ofcom_rsrp_match_min`: Minimum RSRP from OFCOM measurements that match the Vodafone PCI
- `ofcom_rsrp_match_max`: Maximum RSRP from OFCOM measurements that match the Vodafone PCI
- `ofcom_match_count`: Count of OFCOM measurements that match the Vodafone PCI
- `ofcom_rsrp_match_1_mean`: Mean RSRP from OFCOM measurements that match the Vodafone PCI for the top 1 measurement
- `ofcom_rsrp_match_1_min`: Minimum RSRP from OFCOM measurements that match the Vodafone PCI for the top 1 measurement
- `ofcom_rsrp_match_1_max`: Maximum RSRP from OFCOM measurements that match the Vodafone PCI for the top 1 measurement
- `ofcom_match_1_count`: Count of OFCOM measurements that match the Vodafone PCI for the top 1 measurement
- `ofcom_rsrp_all_mean`: Mean RSRP from all OFCOM measurements
- `ofcom_rsrp_all_median`: Median RSRP from all OFCOM measurements
- `ofcom_rsrp_all_min`: Minimum RSRP from all OFCOM measurements
- `ofcom_rsrp_all_max`: Maximum RSRP from all OFCOM measurements
- `ofcom_all_count`: Count of all OFCOM measurements
- `ofcom_rsrp_all_1_mean`: Mean RSRP from all OFCOM measurements for the top 1 measurement
- `ofcom_rsrp_all_1_min`: Minimum RSRP from all OFCOM measurements for the top 1 measurement
- `ofcom_rsrp_all_1_max`: Maximum RSRP from all OFCOM measurements for the top 1 measurement
- `ofcom_all_1_count`: Count of all OFCOM measurements for the top 1 measurement
"""
function summarise_data_2024(data = CSV.read(
        joinpath(
            DATA_PATH, "4g-lte-2024-mobile-signal-measurement-data-combined.csv"),
        DataFrame))
    # A simple data structure to hold *all* the data for a given terrain bin (100m x 100m square)
    AllData = @NamedTuple{x::Int, y::Int, voda_pci::Int, voda_rsrp::Float64,
        ofcom_pci::Vector{Int}, ofcom_rsrp::Vector{Float64}, ofcom_pci_1::Vector{Int},
        ofcom_rsrp_1::Vector{Float64}}
    alldata = Dict{Int, AllData}()
    # Iterate through each row of the data and populate the alldata dictionary
    for row in eachrow(data)
        tb = (row.x - row.x % 100) * 1_000_000 + (row.y - row.y % 100)
        if !haskey(alldata, tb)
            alldata[tb] = AllData((
                row.x - row.x % 100, row.y - row.y % 100, row.voda_pci, row.voda_rsrp,
                Int[], Float64[], Int[], Float64[]))
        end
        if row.ofcom_pci_top1 !== missing
            push!(alldata[tb].ofcom_pci, row.ofcom_pci_top1)
            push!(alldata[tb].ofcom_rsrp, row.ofcom_rsrp_top1)
            # Also store the top 1 PCI and RSRP separately
            push!(alldata[tb].ofcom_pci_1, row.ofcom_pci_top1)
            push!(alldata[tb].ofcom_rsrp_1, row.ofcom_rsrp_top1)
        end
        if row.ofcom_pci_top2 !== missing
            push!(alldata[tb].ofcom_pci, row.ofcom_pci_top2)
            push!(alldata[tb].ofcom_rsrp, row.ofcom_rsrp_top2)
        end
        if row.ofcom_pci_top3 !== missing
            push!(alldata[tb].ofcom_pci, row.ofcom_pci_top3)
            push!(alldata[tb].ofcom_rsrp, row.ofcom_rsrp_top3)
        end
        if row.ofcom_pci_top4 !== missing
            push!(alldata[tb].ofcom_pci, row.ofcom_pci_top4)
            push!(alldata[tb].ofcom_rsrp, row.ofcom_rsrp_top4)
        end
    end
    # A data structure to hold the summary of each terrain bin
    Summary = @NamedTuple{tb::Int, x::Int, y::Int, voda_pci::Int, voda_rsrp::Float64,
        ofcom_pci_mode::Int,
        ofcom_pci_mode_count::Int,
        ofcom_rsrp_match_mean::Union{Float64, Missing},
        ofcom_rsrp_match_median::Union{Float64, Missing},
        ofcom_rsrp_match_min::Union{Float64, Missing},
        ofcom_rsrp_match_max::Union{Float64, Missing},
        ofcom_match_count::Int,
        ofcom_rsrp_match_1_mean::Union{Float64, Missing},
        ofcom_rsrp_match_1_min::Union{Float64, Missing},
        ofcom_rsrp_match_1_max::Union{Float64, Missing},
        ofcom_match_1_count::Int,
        ofcom_rsrp_all_mean::Float64,
        ofcom_rsrp_all_median::Float64,
        ofcom_rsrp_all_min::Float64,
        ofcom_rsrp_all_max::Float64,
        ofcom_all_count::Int,
        ofcom_rsrp_all_1_mean::Union{Float64, Missing},
        ofcom_rsrp_all_1_min::Union{Float64, Missing},
        ofcom_rsrp_all_1_max::Union{Float64, Missing},
        ofcom_all_1_count::Int
    }
    summary = Vector{Summary}()
    # Iterate through each terrain bin and calculate the summary statistics
    for (tb, sum) in alldata
        if isempty(sum.ofcom_pci)
            continue
        end
        x = sum.x
        y = sum.y
        voda_pci = sum.voda_pci

        ofcom_pci = unique(sum.ofcom_pci)
        ofcom_pci_count = zeros(Int, length(ofcom_pci))
        for (i, pci) in enumerate(ofcom_pci)
            ofcom_pci_count[i] = count(sum.ofcom_pci .== pci)
        end
        argmax_pci = argmax(ofcom_pci_count)
        ofcom_pci_mode = ofcom_pci[argmax_pci]
        ofcom_pci_mode_count = ofcom_pci_count[argmax_pci]

        match = sum.ofcom_pci .== voda_pci
        match_1 = sum.ofcom_pci_1 .== voda_pci
        ofcom_rsrp_match = sum.ofcom_rsrp[match]
        ofcom_rsrp_match_1 = sum.ofcom_rsrp_1[match_1]
        ofcom_rsrp_all = sum.ofcom_rsrp
        ofcom_rsrp_all_1 = sum.ofcom_rsrp_1

        ofcom_rsrp_match_mean = isempty(ofcom_rsrp_match) ? missing : mean(ofcom_rsrp_match)
        ofcom_rsrp_match_median = isempty(ofcom_rsrp_match) ? missing :
                                  median(ofcom_rsrp_match)
        ofcom_rsrp_match_min = isempty(ofcom_rsrp_match) ? missing :
                               minimum(ofcom_rsrp_match)
        ofcom_rsrp_match_max = isempty(ofcom_rsrp_match) ? missing :
                               maximum(ofcom_rsrp_match)
        ofcom_match_count = count(match)

        ofcom_rsrp_match_1_mean = isempty(ofcom_rsrp_match_1) ? missing :
                                  mean(ofcom_rsrp_match_1)
        ofcom_rsrp_match_1_min = isempty(ofcom_rsrp_match_1) ? missing :
                                 minimum(ofcom_rsrp_match_1)
        ofcom_rsrp_match_1_max = isempty(ofcom_rsrp_match_1) ? missing :
                                 maximum(ofcom_rsrp_match_1)
        ofcom_match_1_count = count(match_1)

        ofcom_rsrp_all_mean = mean(ofcom_rsrp_all)
        ofcom_rsrp_all_median = median(ofcom_rsrp_all)
        ofcom_rsrp_all_min = minimum(ofcom_rsrp_all)
        ofcom_rsrp_all_max = maximum(ofcom_rsrp_all)
        ofcom_all_count = length(ofcom_rsrp_all)

        ofcom_rsrp_all_1_mean = isempty(ofcom_rsrp_all_1) ? missing : mean(ofcom_rsrp_all_1)
        ofcom_rsrp_all_1_min = isempty(ofcom_rsrp_all_1) ? missing :
                               minimum(ofcom_rsrp_all_1)
        ofcom_rsrp_all_1_max = isempty(ofcom_rsrp_all_1) ? missing :
                               maximum(ofcom_rsrp_all_1)
        ofcom_all_1_count = length(ofcom_rsrp_all_1)

        if ofcom_match_count == ofcom_pci_mode_count
            ofcom_pci_mode = voda_pci  # If the match count is the same as the mode count, it's just a question of sorting so use the voda_pci
        end

        push!(summary,
            Summary((tb, x, y, voda_pci, sum.voda_rsrp,
                ofcom_pci_mode, ofcom_pci_mode_count,
                ofcom_rsrp_match_mean, ofcom_rsrp_match_median,
                ofcom_rsrp_match_min, ofcom_rsrp_match_max, ofcom_match_count,
                ofcom_rsrp_match_1_mean,
                ofcom_rsrp_match_1_min, ofcom_rsrp_match_1_max, ofcom_match_1_count,
                ofcom_rsrp_all_mean, ofcom_rsrp_all_median,
                ofcom_rsrp_all_min, ofcom_rsrp_all_max, ofcom_all_count,
                ofcom_rsrp_all_1_mean,
                ofcom_rsrp_all_1_min, ofcom_rsrp_all_1_max, ofcom_all_1_count
            ))
        )
    end
    CSV.write(
        joinpath(DATA_PATH, "4g-lte-2024-mobile-signal-measurement-data-summary.csv"),
        summary
    )
    return summary
end

# Run the functions

combine_data_2024()
summarise_data_2024()
