from datetime import datetime
import time

import pandas as pd
import typer

import subprocess
import numpy as np
import random
import os.path

import opendp.smartnoise.core as sn
from snsql.sql.reader import PandasReader
from snsql.sql import PrivateReader
from snsql import Privacy

import noise_mechanism as nm

#### Defaults for loading data
DEFAULT_DATA_DIRECTORY = "./tests/data"
DEFAULT_FIPS = "36"  # New York
DEFAULT_START_DATE = datetime(2020,8,15)
DEFAULT_END_DATE = datetime(2020,11,16)

#### Defaults for storing noisy data
DEFAULT_NOISY_DATA_DIRECTORY = "./data/mob-dp"

#### Defaults for running the R model
DEFAULT_ITERATIONS = 1000
DEFAULT_MECHANISM = "laplace"
DEFAULT_EPSILON = 0.1
DEFAULT_DELTA = 10e-6
DEFAULT_MODEL_OUTPUT_DIRECTORY = './data/model-outputs'

app = typer.Typer()


def load(
    data_dir: str = DEFAULT_DATA_DIRECTORY, fips: str = DEFAULT_FIPS, start_date: datetime = DEFAULT_START_DATE, end_date: datetime = DEFAULT_END_DATE
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

def add_noise_to_df(df: pd.DataFrame, mechanism: str = DEFAULT_MECHANISM, epsilon: float = DEFAULT_EPSILON, delta: float = DEFAULT_DELTA):
        
    meta = './init/transitions_with_censor_false.yaml'

    #Reading the DF
    reader_censored = PandasReader(df, meta)

    query = "SELECT start_of_block, from_state_fips, from_county_fips, to_state_fips, to_county_fips, activity_day, COUNT(transitions) \
    AS transitions from mobility.activity GROUP BY start_of_block, from_state_fips, from_county_fips, to_state_fips, to_county_fips, activity_day \
    ORDER BY start_of_block, from_state_fips, from_county_fips, to_state_fips, to_county_fips, activity_day"
    
    # Format what the sensitive output from the SQL query should look like
    df_pre_agg = df[['start_of_block','from_state_fips', 'from_county_fips', 'to_state_fips','to_county_fips', 'activity_day']]
    
    # The number of unique contributors to each cell is the same as the number of transitions
    df_pre_agg['COUNT ( transitions )'] = df['transitions']
    
    #df_pre_agg['COUNT ( * )'] = df['transitions']
    pre_aggregated = [tuple(df_pre_agg.columns.values), *df_pre_agg.itertuples(index=False, name=None)]
    
    #Passing reader to private reader
    private_reader_censor = PrivateReader(reader_censored, meta, privacy=Privacy(epsilon=float(epsilon), delta=float(delta)))
    
    result_dp = private_reader_censor.execute(query, pre_aggregated=pre_aggregated)
    
    result_dp = pd.DataFrame(result_dp[1:], columns=['start_of_block','from_state_fips', 'from_county_fips','to_state_fips', 'to_county_fips', 'activity_day','transitions'])
    
    return result_dp


def call_r_model(mechanism: str = DEFAULT_MECHANISM, epsilon: int = DEFAULT_EPSILON, iterations: int = DEFAULT_ITERATIONS, output: str = DEFAULT_MODEL_OUTPUT_PATH):
    
    total = iterations
    typer.echo(f"Calling R model over {iterations} iterations")
    with typer.progressbar(range(iterations)) as steps:
        try:
            subprocess.call (["Rscript", "--vanilla", "--no-environ", "./pipeline.R", "--args", "{}".format(mechanism), "{}".format(epsilon), "{}".format(iterations), "{}".format(output)])
        except Exception as e:
            print(e)
            exit(1)
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
    data_dir: str = typer.Argument(
        default=DEFAULT_DATA_DIRECTORY,
        help="The path to the data directory with data in Parquet format with Hive folders",
    ),
    fips: str = typer.Option(
        default=DEFAULT_FIPS,
        help="The state to filter data to and from, default 36 for NY",
    ),
    start_date: datetime = typer.Option(
        default=DEFAULT_START_DATE,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The start date to filter by, inclusive",
    ),
    end_date: datetime = typer.Option(
        default=DEFAULT_END_DATE,
        formats=["%Y-%m-%d", "%Y%m%d"],
        help="The end date to filter by, exclusive",
    ),
    mechanism: str = typer.Argument(
        default=DEFAULT_MECHANISM,
        help="The default noise type you would like to add to the main dataset. Current implementations only avaialble for 'laplace' and 'gaussian'",
    ),
    epsilon: str = typer.Argument(
        default=DEFAULT_EPSILON,
        help="Specify the privacy budget",
    ),
    delta: str = typer.Argument(
        default=DEFAULT_DELTA,
        help="Specify the privacy delta",
    ),
    iterations: int = typer.Argument(
        default=DEFAULT_ITERATIONS,
        help="The number of noisy data outputs to create",
    ) 
    #start_date: datetime = typer.Option(
    #    default=None,
    #    formats=["%Y-%m-%d", "%Y%m%d"],
    #    help="The start date to filter by, inclusive",
    #),
    #end_date: datetime = typer.Option(
    #    default=None,
    #    formats=["%Y-%m-%d", "%Y%m%d"],
    #    help="The end date to filter by, exclusive",
    #)
):
    """
    Reads already noisy data, repeatedly adds more noise using the OpenDP libraries, then calls the R
    functions for simulation.

    The data path should have directories named `activity_day=%Y-%m-%d` in it.

    If no start or end dates are specified, it will use the whole directory
    """
    #df = load(data_path, fips, start_date, end_date)
    #df = noisy_df(df, iterations)
    df = load(data_dir, fips, start_date, end_date)

    for iteration in range(1, iterations+1):
        noisy_df_output = add_noise_to_df(df, mechanism,epsilon, delta)
        noisy_data_output_directory = '{}/noise_type={}/ep={}/iteration={}.csv'.format(DEFAULT_NOISY_DATA_DIRECTORY, mechanism, epsilon,iteration)
        noisy_df_output.to_csv(noisy_data_output_directory)

    call_r_model(mechanism, epsilon, iterations, output)


if __name__ == "__main__":
    app()
