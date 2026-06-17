import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import re
from pathlib import Path

def read_phreeqc_output(filepath):
    """
    Read PHREEQC selected output file with quoted column headers
    
    Parameters:
    filepath: path to the .sel file
    
    Returns:
    DataFrame with parsed data
    """
    with open(filepath, 'r') as f:
        # Read the first line (headers)
        header_line = f.readline().strip()
        
        # Parse headers: handle quoted strings and spaces
        # Find all quoted strings and split by spaces outside quotes
        headers = []
        # Pattern to match either quoted strings or non-quoted tokens
        pattern = r'"([^"]*)"|([^\s]+)'
        matches = re.findall(pattern, header_line)
        
        for match in matches:
            # match[0] is quoted content, match[1] is unquoted token
            header = match[0] if match[0] else match[1]
            headers.append(header)
        
        # Read the data
        data_lines = []
        for line in f:
            line = line.strip()
            if line and not line.startswith('#'):  # Skip empty lines and comments
                # Split by whitespace
                values = line.split()
                data_lines.append(values)
        
        # Convert to DataFrame
        df = pd.DataFrame(data_lines, columns=headers)
        
        # Convert numeric columns to float (handle potential missing values)
        for col in df.columns:
            try:
                df[col] = pd.to_numeric(df[col], errors='coerce')
            except:
                pass
        
        return df

def compare_phreeqc_results(output_dirs, variables_to_plot, x_variable='time_h', \
                            x_range=None, y_range=None):
    """
    Compare PHREEQC output files from different directories
    
    Parameters:
    output_dirs: dict with keys as labels and values as paths to .sel files
    variables_to_plot: list of variable names to plot
    x_variable: variable to use for x-axis
    """
    # Read all output files
    data = {}
    for label, filepath in output_dirs.items():
        try:
            df = read_phreeqc_output(filepath)
            data[label] = df
            print(f"Successfully read {label}: {filepath}")
            print(f"  Shape: {df.shape}")
            print(f"  Columns: {list(df.columns)}")
            print()
        except Exception as e:
            print(f"Error reading {label}: {e}")
    
    # Determine subplot layout
    n_vars = len(variables_to_plot)
    n_cols = 3
    n_rows = (n_vars + n_cols - 1) // n_cols
    
    # Create figure with subplots
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(15, 4*n_rows))
    axes = axes.flatten() if n_rows * n_cols > 1 else [axes]
    
    # Hide unused subplots
    for i in range(n_vars, len(axes)):
        axes[i].set_visible(False)
    
    # Define colors and markers for different datasets
    colors = {'a': 'blue', 'b': 'red', 'c': 'green', 'd': 'orange'}
    markers = {'a': 'o', 'b': 's', 'c': '^', 'd': 'd'}
    labels = {'a': 'DDLM', 'b': 'Donnan thickness 1.0e-6', \
              'c': 'Donnan thickness 1.0e-7', 'd': 'Donnan thickness 1.0e-8'}
    
    
    # Plot each variable
    for idx, var in enumerate(variables_to_plot):
        ax = axes[idx]
        
        for label, df in data.items():
            if var in df.columns and x_variable in df.columns:
                # Get data, drop NaN values
                valid_data = df[[x_variable, var]].dropna()
                x = valid_data[x_variable]
                y = valid_data[var]
                
                if len(x) > 0:
                    ax.plot(x, y, 
                           marker=markers.get(label, 'o'), 
                           markersize=4,
                           linewidth=1,
                           label=labels.get(label, 'unknown'),
                           color=colors.get(label, 'gray'),
                           alpha=0.8, 
                           markevery=int(len(x)/10))
        
        # Set axis ranges if specified
        if x_range is not None:
            ax.set_xlim(x_range)
        if y_range is not None:
            ax.set_ylim(y_range)
            
        ax.set_xlabel(x_variable)
        ax.set_ylabel(var)
        ax.set_title(f'{var} vs {x_variable}')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Format x-axis if it's time
        if 'time' in x_variable.lower():
            ax.set_xlabel(f'{x_variable} (hours)')
    
    plt.tight_layout()
    return fig

# Main execution
if __name__ == "__main__":
    # Define the output files
    output_files = {
        'a': '../phreeqc-simulation/phreeqc_ddlm/surfx_edl.sel',
        'b': '../phreeqc-simulation/phreeqc_donnan_thickness_1.0e-6/surfx_edl.sel',
        'c': '../phreeqc-simulation/phreeqc_donnan_thickness_1.0e-7/surfx_edl.sel',
        'd': '../phreeqc-simulation/phreeqc_donnan_thickness_1.0e-8/surfx_edl.sel'
    }
    
    ######################################################################################
    # Define variables to plot
    ######################################################################################
    variables_to_plot = [
        "so4-2", "co3-2", "ca+2", "mg+2", 
        "po4-3", "h4sio4", "cl-1", "na+1", 
        "charge_balance[%]"
    ]
    
    # Figure to be saved
    fig_file = 'phreeqc_gbt_ddlm_vs_donnan.png'
    
    # Read and compare data
    data = {}
    for label, filepath in output_files.items():
        try:
            df = read_phreeqc_output(filepath)
            data[label] = df
            print(f"Successfully read {label}: {filepath}")
            print(f"  Shape: {df.shape}")
            print(f"  Columns: {list(df.columns)}")
            print()
        except Exception as e:
            print(f"Error reading {label}: {e}")
    
    if data:
        # Create comparison plot
        fig = compare_phreeqc_results(output_files, variables_to_plot, x_variable='time_h', \
                                      x_range=(0, 80))
        
        # Save the figure
        plt.savefig(fig_file, dpi=300, bbox_inches='tight')
        #plt.show()        

    else:
        print("No data files could be read. Please check file paths.")
        
    ######################################################################################
    # Define variables to plot
    ######################################################################################
    variables_to_plot = ["=feoh2+(w)", "=feoh(w)", "=feh3sio4(w)", "=fehco3(w)", "=feh2sio4-w", "=fehpo4-(w)", \
                         "=feopo4-2(w)", "=feco3-(w)", "=feo-(w)", "=fehsio4-2(w)", "=feh2po4(w)", "=feohso4-2(w)", \
                         "=feso4-(w)", "=feomg+(w)", "=feoca+(w)", "=feoh(s)", "=feohca+2(s)", "=feo-(s)", "=feoh2+(s)"]
    
    # Figure to be saved
    fig_file = 'phreeqc_gbb_ddlm_vs_donnan.png'
    
    # Read and compare data
    data = {}
    for label, filepath in output_files.items():
        try:
            df = read_phreeqc_output(filepath)
            data[label] = df
            print(f"Successfully read {label}: {filepath}")
            print(f"  Shape: {df.shape}")
            print(f"  Columns: {list(df.columns)}")
            print()
        except Exception as e:
            print(f"Error reading {label}: {e}")
    
    if data:
        # Create comparison plot
        fig = compare_phreeqc_results(output_files, variables_to_plot, x_variable='time_h', \
                                      x_range=(0, 80))
        
        # Save the figure
        plt.savefig(fig_file, dpi=300, bbox_inches='tight')
        #plt.show()        

    else:
        print("No data files could be read. Please check file paths.")