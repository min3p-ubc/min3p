import numpy as np
import matplotlib.pyplot as plt
import re
from matplotlib.ticker import ScalarFormatter

def read_tecplot_file(filename):
    """
    Read Tecplot format file and return data as dictionary.
    Handles variable names in second line with quotes.
    """
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    # Find the line with 'variables ='
    var_line = None
    data_start_line = 0
    
    for i, line in enumerate(lines):
        if 'variables' in line.lower() and '=' in line:
            var_line = line
            data_start_line = i + 1
            break
    
    if var_line is None:
        raise ValueError(f"Could not find 'variables =' line in {filename}")
    
    # Extract variable names from the variables line
    # Remove 'variables =' part
    var_part = var_line.split('=', 1)[1].strip()
    
    # Find all quoted strings
    var_names = re.findall(r'"([^"]*)"', var_part)
    
    if not var_names:
        # If no quotes found, try splitting by comma/space
        var_names = [v.strip() for v in re.split(r'[,\s]+', var_part) if v.strip()]
    
    # Read data from fourth line (index 3) or from data_start_line
    # Some files might have comments, so we'll start from data_start_line
    data_lines = lines[data_start_line:]
    
    # Parse data
    data = {name: [] for name in var_names}
    
    for line in data_lines:
        if line.strip() and not line.strip().startswith('#'):
            values = line.strip().split()
            if len(values) == len(var_names):
                for i, name in enumerate(var_names):
                    try:
                        data[name].append(float(values[i]))
                    except ValueError:
                        data[name].append(values[i])
    
    # Convert to numpy arrays
    for name in data:
        if data[name] and isinstance(data[name][0], (int, float)):
            data[name] = np.array(data[name])
    
    return data, var_names

def force_scientific_notation(ax, axis='both'):
    """
    Force scientific notation on specified axis/axes.
    
    Parameters:
    ax: matplotlib axis object
    axis: 'x', 'y', or 'both' to specify which axes to format
    """
    if axis == 'x' or axis == 'both':
        # Force scientific notation for x-axis
        ax.xaxis.set_major_formatter(ScalarFormatter(useMathText=True))
        ax.ticklabel_format(axis='x', style='sci', scilimits=(0, 0), useMathText=True)
    
    if axis == 'y' or axis == 'both':
        # Force scientific notation for y-axis
        ax.yaxis.set_major_formatter(ScalarFormatter(useMathText=True))
        ax.ticklabel_format(axis='y', style='sci', scilimits=(0, 0), useMathText=True)

def plot_variables(result1_file, result2_file, result_figure, variables_to_plot, z_variable='z', n_cols=2, figsize_per_plot=(5, 4)):
    """
    Plot specified variables against z for two result files.
    
    Parameters:
    result1_file, result2_file: paths to the Tecplot files
    variables_to_plot: list of variable names to plot
    z_variable: name of the z-variable (default: 'z')
    n_cols: number of columns in subplot grid (default: 2)
    figsize_per_plot: tuple (width, height) for each subplot (default: (5, 4))
    """
    # Read data from both files
    print(f"Reading {result1_file}...")
    data1, vars1 = read_tecplot_file(result1_file)
    print(f"Variables in {result1_file}: {vars1}")
    
    print(f"\nReading {result2_file}...")
    data2, vars2 = read_tecplot_file(result2_file)
    print(f"Variables in {result2_file}: {vars2}")
    
    # Check if z variable exists in both files
    if z_variable not in data1:
        raise ValueError(f"'{z_variable}' not found in {result1_file}")
    if z_variable not in data2:
        raise ValueError(f"'{z_variable}' not found in {result2_file}")
    
    # Get z data
    z1 = data1[z_variable]
    z2 = data2[z_variable]
    
    # Calculate grid dimensions
    n_vars = len(variables_to_plot)
    n_rows = int(np.ceil(n_vars / n_cols))
    
    # Create figure with subplots
    fig_width = figsize_per_plot[0] * n_cols
    fig_height = figsize_per_plot[1] * n_rows
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(fig_width, fig_height))
    
    # Flatten axes array for easy indexing (handle both 1D and 2D cases)
    if n_rows == 1 and n_cols == 1:
        axes = np.array([axes])
    elif n_rows == 1 or n_cols == 1:
        axes = axes.flatten()
    else:
        axes = axes.flatten()
    
    # Plot each variable
    for idx, var in enumerate(variables_to_plot):
        ax = axes[idx]
        
        # Check if variable exists in each file
        if var in data1:
            ax.plot(z1, data1[var], 'r-', marker='*', markersize=6, 
                   linewidth=1.0, label='min3p', alpha=0.7, markevery=int(len(z1)/8))
        else:
            print(f"Warning: '{var}' not found in {result1_file}")
            ax.text(0.5, 0.5, f'{var} not in result1', 
                   transform=ax.transAxes, ha='center', va='center')
        
        if var in data2:
            ax.plot(z2, data2[var], 'b-', marker='s', markersize=5, 
                   linewidth=1.0, label='phreeqc', alpha=0.7, markevery=int(len(z2)/10))
        else:
            print(f"Warning: '{var}' not found in {result2_file}")
            if var not in data1:
                ax.text(0.5, 0.5, f'{var} not in either file', 
                       transform=ax.transAxes, ha='center', va='center')
        
        # Force scientific notation for both axes
        force_scientific_notation(ax, axis='both')
        
        ax.set_xlabel(z_variable)
        ax.set_ylabel(var)
        ax.set_title(f'{var} vs {z_variable}')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Invert y-axis if z typically increases downward (optional)
        # ax.invert_yaxis()
    
    # Hide any unused subplots
    for idx in range(n_vars, len(axes)):
        axes[idx].set_visible(False)
    
    plt.tight_layout()
    plt.savefig(result_figure, dpi=300, bbox_inches='tight')    
    #plt.show()

# Example usage
if __name__ == "__main__":
     ## Spatial output after 12.7 hours
    # Aqueous sepcies
    variables_to_plot = ["so4-2", "co3-2", "ca+2", "mg+2", "po4-3", "h4sio4", "cl-1", "na+1"]

    # Surface complexation without electrostatic
    file_min3p = "../min3p-simulation/surface_hfo_s_w/cdr_simulation_1.gst"
    file_phreeqc = "../phreeqc-simulation/phreeqc/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_1_gst.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=3, figsize_per_plot=(4.5, 4))    
   
    # Surface complexation with CCM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ccm/cdr_simulation_1.gst"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ccm/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_ccm_1_gst.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=3, figsize_per_plot=(4.5, 4))
    
    # Surface complexation with DDLM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ddlm/cdr_simulation_1.gst"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ddlm/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_ddlm_1_gst.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=3, figsize_per_plot=(4.5, 4))
    
    # Sorbed species
    variables_to_plot = ["=feoh2+(w)", "=feoh(w)", "=feh3sio4(w)", "=fehco3(w)", "=feh2sio4-w", "=fehpo4-(w)", \
                         "=feopo4-2(w)", "=feco3-(w)", "=feo-(w)", "=fehsio4-2(w)", "=feh2po4(w)", "=feohso4-2(w)", \
                         "=feso4-(w)", "=feomg+(w)", "=feoca+(w)", "=feoh(s)", "=feohca+2(s)", "=feo-(s)", "=feoh2+(s)"]

    # Surface complexation without electrostatic
    file_min3p = "../min3p-simulation/surface_hfo_s_w/cdr_simulation_1.gsb"
    file_phreeqc = "../phreeqc-simulation/phreeqc/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_1_gsb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=5, figsize_per_plot=(4.5, 4))    
   
    # Surface complexation with CCM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ccm/cdr_simulation_1.gsb"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ccm/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_ccm_1_gsb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=5, figsize_per_plot=(4.5, 4))
    
    # Surface complexation with DDLM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ddlm/cdr_simulation_1.gsb"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ddlm/cdr_simulation_phreeqc_1.gstb"
    file_fig = "./surface_hfo_s_w_ddlm_1_gsb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='z', n_cols=5, figsize_per_plot=(4.5, 4))


    ## Transient output at outflux boundary
    # Aqueous sepcies
    variables_to_plot = ["so4-2", "co3-2", "ca+2", "mg+2", "po4-3", "h4sio4", "cl-1", "na+1"]
    # Surface complexation without electrostatic
    file_min3p = "../min3p-simulation/surface_hfo_s_w/cdr_simulation_1.gbt"
    file_phreeqc = "../phreeqc-simulation/phreeqc/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_1_gbt.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=3, figsize_per_plot=(4.5, 4))    
   
    # Surface complexation with CCM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ccm/cdr_simulation_1.gbt"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ccm/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_ccm_1_gbt.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=3, figsize_per_plot=(4.5, 4))
    
    # Surface complexation with DDLM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ddlm/cdr_simulation_1.gbt"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ddlm/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_ddlm_1_gbt.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=3, figsize_per_plot=(4.5, 4))
    
    # Sorbed species
    variables_to_plot = ["=feoh2+(w)", "=feoh(w)", "=feh3sio4(w)", "=fehco3(w)", "=feh2sio4-w", "=fehpo4-(w)", \
                         "=feopo4-2(w)", "=feco3-(w)", "=feo-(w)", "=fehsio4-2(w)", "=feh2po4(w)", "=feohso4-2(w)", \
                         "=feso4-(w)", "=feomg+(w)", "=feoca+(w)", "=feoh(s)", "=feohca+2(s)", "=feo-(s)", "=feoh2+(s)"]

    # Surface complexation without electrostatic
    file_min3p = "../min3p-simulation/surface_hfo_s_w/cdr_simulation_1.gbb"
    file_phreeqc = "../phreeqc-simulation/phreeqc/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_1_gbb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=5, figsize_per_plot=(4.5, 4))    
   
    # Surface complexation with CCM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ccm/cdr_simulation_1.gbb"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ccm/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_ccm_1_gbb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=5, figsize_per_plot=(4.5, 4))
    
    # Surface complexation with DDLM
    file_min3p = "../min3p-simulation/surface_hfo_s_w_ddlm/cdr_simulation_1.gbb"
    file_phreeqc = "../phreeqc-simulation/phreeqc_ddlm/cdr_simulation_phreeqc_1.gbtb"
    file_fig = "./surface_hfo_s_w_ddlm_1_gbb.png"
    plot_variables(file_min3p, file_phreeqc, file_fig, variables_to_plot, z_variable='time', n_cols=5, figsize_per_plot=(4.5, 4))
    
