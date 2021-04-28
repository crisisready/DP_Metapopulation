from datetime import datetime
import time

import pandas as pd
import typer

import subprocess
import numpy as np
import random

DEFAULT_DATA_DIRECTORY = "./tests/data"
DEFAULT_FIPS = "36"  # New York
DEFAULT_ITERATIONS = 100

app = typer.Typer()


def load(
    data_dir: str, fips: str, start_date: datetime, end_date: datetime
) -> pd.DataFrame:
    """
    Reads data from the data directory, assuming that the format is a bunch of subdirectories
    in the form `activity_day=%Y-%m-%d` which will be converted to a Python date in the resulting
    data frame.
    """
    typer.echo(f"Reading data from {data_dir}")
    df = pd.read_parquet(data_dir)
    # Filter first, then convert to save some compute.
    # This cases activity_day to string, but it *may* be faster to convert to a list of
    # categories that match the dates.
    if start_date:
        str_start_date = start_date.strftime("%Y-%m-%d")
        typer.echo(f"Filtering start date after {str_start_date}")
        df.drop(df[df["activity_day"].astype(str) < str_start_date].index, inplace=True)
    if end_date:
        str_end_date = end_date.strftime("%Y-%m-%d")
        typer.echo(f"Filtering start date strictly before {str_end_date}")
        df.drop(df[df["activity_day"].astype(str) >= str_end_date].index, inplace=True)
    typer.echo("Filtering by state")
    df.drop(
        df[(df["from_state_fips"] != fips) | (df["to_state_fips"] != fips)].index,
        inplace=True,
    )
    df["activity_day"] = df["activity_day"].apply(
        lambda str_day: datetime.strptime(str_day, "%Y-%m-%d").date()
    )
    return df

def noisy_df(df: pd.DataFrame, iterations: int = 100):
    
    dlen = len(df)
    n = []
    typer.echo(f"Applying DP noise on columns over {iterations} iterations")
    with typer.progressbar(range(iterations)) as steps:
        for _ in steps:
          noisy_val = df.transitions.values*random.random()
          n = np.vstack(noisy_val)
        m = np.average(n, axis=0)
        df['transitions']=pd.Series(m)
        return df

def call_r_model(df: pd.DataFrame, iterations: int):
    
    typer.echo(f"Calling R model over {iterations}")
    with typer.progressbar(range(iterations)) as steps:
        for _ in steps:
            subprocess.call (["Rscript", "--vanilla", "./src/spatial_seir.R"])
            time.sleep(0.05)


@app.command()
def pull(
    base_url: str = typer.Argument(
        ..., help="The cloud URL to pull transition data from"
    ),
    output_url: str = typer.Argument(
        default=DEFAULT_DATA_DIRECTORY, help="The data directory to write to"
    ),
    start_date: datetime = typer.Option(
        default=None,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The start date to filter by, inclusive",
    ),
    end_date: datetime = typer.Option(
        default=None,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The end date to filter by, exclusive",
    ),
):
    """
    Pulls down data from a remote data source, given a base URL.

    Currently unimplemented.
    """
    typer.echo(
        typer.style("This is not yet implemented", fg=typer.colors.RED), err=True
    )
    exit(1)


@app.command()
def simulate(
    data_path: str = typer.Argument(
        default=DEFAULT_DATA_DIRECTORY,
        help="The path to the data directory with data in Parquet format with Hive folders",
    ),
    fips: str = typer.Option(
        default=DEFAULT_FIPS,
        help="The state to filter data to and from, default 36 for NY",
    ),
    iterations: int = typer.Option(
        default=DEFAULT_ITERATIONS,
        help="The number of noisy data outputs to create",
    ),
    start_date: datetime = typer.Option(
        default=None,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The start date to filter by, inclusive",
    ),
    end_date: datetime = typer.Option(
        default=None,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The end date to filter by, exclusive",
    ),
):
    """
    Reads already noisy data, repeatedly adds more noise using the OpenDP libraries, then calls the R
    functions for simulation.

    The data path should have directories named `activity_day=%Y-%m-%d` in it.

    If no start or end dates are specified, it will use the whole directory
    """
    df = load(data_path, fips, start_date, end_date)
    df = noisy_df(df, iterations)
    call_r_model(df, iterations)


if __name__ == "__main__":
    app()
