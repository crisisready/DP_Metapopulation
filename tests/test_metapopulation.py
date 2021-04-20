from datetime import date
from pathlib import Path

import metapopulation as mp

DATA_DIR = str(Path() / "tests" / "data")
AVAILABLE_DATES = [date(2021, 4, day) for day in range(1, 15, 1)]


def test_activity_day_date_type():
    df = mp.load(DATA_DIR, fips=mp.DEFAULT_FIPS, start_date=None, end_date=None)
    assert isinstance(df["activity_day"].iloc[0], date)


def test_no_date_filter():
    df = mp.load(DATA_DIR, fips=mp.DEFAULT_FIPS, start_date=None, end_date=None)
    dates = df["activity_day"].unique().tolist()
    assert set(dates) == set(AVAILABLE_DATES)


def test_filter_by_date():
    df = mp.load(
        DATA_DIR,
        fips=mp.DEFAULT_FIPS,
        start_date=date(2021, 4, 2),
        end_date=date(2021, 4, 7),
    )
    dates = df["activity_day"].unique().tolist()
    # Note that this is exclusive of the end, inclusive of the start, just like range
    assert set(dates) == set([date(2021, 4, day) for day in range(2, 7)])


def test_filter_by_fips():
    df = mp.load(DATA_DIR, fips=mp.DEFAULT_FIPS, start_date=None, end_date=None)
    assert (
        df[
            (df["to_state_fips"] != mp.DEFAULT_FIPS)
            & (df["from_state_fips"] != mp.DEFAULT_FIPS)  # noqa: W503
        ].size
        == 0  # noqa: W503
    )


def test_filter_by_nondefault_fips():
    df = mp.load(DATA_DIR, fips="13", start_date=None, end_date=None)
    assert (
        df[
            (df["to_state_fips"] != "13")
            & (df["from_state_fips"] != "13")  # noqa: W503
        ].size
        == 0  # noqa: W503
    )
