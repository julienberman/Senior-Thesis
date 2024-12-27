## Workflow

I have set up this repository using the [research template](https://github.com/JMSLab/Template) developped by Jesse Shapiro. The flow supports R, Stata, Python, and Matlab.


### Repository Structure

- `source/` contains source scripts and raw data. 
- `output/` mimics the folder structure in `source/`, and is designed for all output, including figures, tables, and cleaned data. 
    - Every file in these folders is produced by `source/`.
- `temp/` is used by scripts to store temporary or intermediate files.
- Issue folders are created  when working on specific issues or tasks related to the project. The folder structure contains all code and deliverables relevant to the issue. Once the task is resolved, the folder is deleted and not merged into the main branch.

### SCons

SCons is a software construction tool (akin to Make but more powerful and Python-based) that automates the build process of complex projects. It helps manage dependencies and ensures that the necessary files are produced or updated in the correct order, based on what has changed in the project.

SConscript files are used to specify what outputs should be built and the dependencies needed for each target. For instance, to use the script `source/derived/script.py` and `source/raw_data/raw.csv` to generate a file like `output/derived/filename.csv`, simply add an entry to the relevant SConscript file in `source/derived/` that specifies the target file, source script, and any additional dependencies. In this case, run:

```python
target = ['#output/derived/filename.csv']
source = ['#source/derived/example.py',
          '#source/raw_data/raw.csv']
env.Python(target, source)
```
- `target` is a list with all of the files produced by the script.

- `source` is a list with the script's name and all of the files used as input; the script _must_ be the first element of the list.

- `env.Python` is the Python builder provided in `source/lib/JMSLab/builders`; this is imported and saved as part of the `env` object in the `SConstruct` file at the root of the project.

SCons uses custom builders (written in Python) to execute the scripts. Running `scons` in the command-line from the project root automatically builds all the targets, running the scripts and commands necessary to generate the required outputs. To build a specific file or run a specific script, you can call it directly by running scons path/to/target, which will build all dependencies for that target.

### Quick Start

For Conda/Micromamba Users:

- Install [conda](https://docs.conda.io/en/latest/miniconda.html) or [micromamba](https://mamba.readthedocs.io/en/latest/installation.html).

- Navigate to the project root folder.

- Create and activate the environment:

```bash
conda env create -f source/lib/environment.yml
conda activate political-advertising
```

```bash
micromamba create -f source/lib/environment.yml
micromamba activate political-advertising
```

- To update the environment:
```bash
micromamba install -f source/lib/environment.yml
```

For pip users:
- Ensure you have Python installed (version 3.10 or higher)

- Navigate to the project root folder

- Install the necessary depndencies by running:
    ```
    pip install -r source/lib/requirements.txt
    ```
- If there are requirements for other languages, they will be located in `source/lib/requirements.{ext}`, with `{ext}` equal to `do`, `r`, `m`, and so on, depending on the language.

- Make sure that all required program executables are in the the system path.

- To compile the project, open the command-line and run `scons` from the project's root folder. To run a given script or create a given file, run `scons path/to/script_or_file`; this will recursively run all the dependencies required to run the script or create the file.

## Usage


