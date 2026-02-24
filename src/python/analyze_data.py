


folder_processed_files = os.path.join(os.getcwd(), "data/processed")


tc_palette = [
    "#FF412C",
    "#31CAA8",
    "#06A6EE",
]
tsv_palette = [
    "#06A6EE",
    "#06A6EE",
    "#31CAA8",
    "#31CAA8",
    "#31CAA8",
    "#FF412C",
    "#FF412C",
]
ps_core = ["#FF412C", "#9F29FF", "#FF2380", "#06A6EE", "#31CAA8"]
secondary_core = ["#9F29FF", "#FF2380", "#06A6EE", "#31CAA8"]
grapefruit = ["#FF8D80", "#FF6756", "#FF412C", "#E8281E", "#C20800"]
plum = ["#C57FFF", "#B254FF", "#9F29FF", "#8001D8", "#6600AB"]
pomegranate = ["#FF7BB3", "#FF4F99", "#FF2380", "#CD0E28", "#A50115"]
blueberry = ["#6ACAF5", "#38B8F1", "#06A6EE", "#006BDB", "#003CAB"]
avocado = ["#83DFCB", "#5AD5B9", "#31CAA8", "#149678", "#006B46"]
full = [
    "#FF412C",
    "#9F29FF",
    "#FF2380",
    "#06A6EE",
    "#31CAA8",
    "#FFBA22",
    "#6600AB",
    "#FF7BB3",
    "#003CAB",
    "#83DFCB",
    "#C20800",
    "#C57FFF",
    "#FF8D80",
    "#6ACAF5",
    "#006B46",
    "#FF6756",
    "#8001D8",
    "#FF4F99",
    "#006BDB",
    "#5AD5B9",
    "#E8281E",
    "#B254FF",
    "#149678",
    "#FFE146",
]
grayscale = [
    "#F2F2F2",
    "#CCD1DB",
    "#B3BBC9",
    "#99A4B7",
    "#808DA5",
    "#616E90",
    "#33415C",
    "#000000",
]

# sns.palplot(grapefruit)

map_col_names = {
    "TP": "Thermal Preference (TP)",
    "TSV": "Thermal Sensation Vote (TSV)",
    "ta": r"$t_a$ (°C)",
    "tr": r"$t_r$ (°C)",
    "co2": r"CO$_2$ (ppm)",
    "va": r"$V$ (m/s)",
    "rh": "RH (\%)",
    "t-sk": r"$t_sk$ (°C)",
    "Result": "SBB Cognitive Test score",
    "Condition": "Target room temperature",
    "Age": "Age (y)",
    "Height": "Height (m)",
    "Weight": "Weight (kg)",
    "BMI": "BMI (kg/m$^2$)",
}

sorted_col = {
    "TSV": [-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0],
    "TP": ["Cooler", "No change", "Warmer"],
    "Acceptability": [
        "VERY UNACCEPTABLE",
        "SOMEWHAT UNACCEPTABLE",
        "NEITHER ACCEPTABLE NOR UNACCEPTABLE",
        "SOMEWHAT ACCEPTABLE",
        "VERY ACCEPTABLE",
    ],
}


def import_merge_df(load_saved_file=False):
    if load_saved_file:  # import preprocessed csv file to speed up analysis
        return pd.read_pickle(
            os.path.join(folder_processed_files, "df_ct_q.pkl.gzip"), compression="gzip"
        )

    # read Qualtrics data
    _df_q = pd.read_csv(
        os.path.join(folder_processed_files, "qualtrics.csv.gzip"),
        compression="gzip",
        index_col="timestamp",
    )
    _df_q.index = pd.to_datetime(_df_q.index)
    _df_q["Date"] = _df_q.index.date
    _df_q["Session"] = ["Aft" if x > 13 else "Mor" for x in _df_q.index.hour]
    _df_q["TP"] = _df_q["TP"].str.capitalize()

    # read environmental data
    _df_env = pd.read_csv(
        os.path.join(folder_processed_files, "df_env_combined.csv.gzip"),
        compression="gzip",
        index_col="timestamp",
    )

    _df_env.index = pd.to_datetime(_df_env.index, errors="coerce", dayfirst=True)

    # read iButton data and merge with Qualtrics data
    # df_ib = pd.read_pickle(
    #     os.path.join(folder_processed_files, "ibutton.pkl.gzip"),
    #     compression="gzip",
    # ).rename(columns={"tmp": "t-sk"})
    # _df_q = pd.merge_asof(
    #     _df_q.sort_index(),
    #     df_ib.sort_index(),
    #     left_index=True,
    #     right_index=True,
    #     tolerance=pd.Timedelta("10 min"),
    #     by="user-id",
    # )

    # import info on in which room participants were sitting
    # _df_room = pd.read_csv(
    #     os.path.join(folder_processed_files, "Room.csv"),
    # )
    # _df_room["Date"] = pd.to_datetime(_df_room["TimeStamp"], format="%d/%m/%Y").astype(
    #     "str"
    # )
    # _df_room["Session"] = _df_room["Schedule"].str[:3]
    # _df_room["Room"] = [f"Room {x}" for x in _df_room["Room"]]
    # _df_room = _df_room[["ID", "Room", "Date", "Session"]]
    # _df_room = _df_room.rename(columns={"ID": "user-id"})

    # import cognitive test
    _df_ct = pd.read_csv(
        os.path.join(folder_processed_files, "cat_test_scores.csv"),
    )
    _df_ct["Result"] = _df_ct["test_cat_score"]
    _df_ct["Condition"] = _df_ct["session-type"].str.replace("C", "").astype("int")
    _df_ct["Timestamp"] = _df_ct["Timestamp"].str.replace(
        r"\s[A-Z]{3}$", "", regex=True
    )
    _df_ct.index = pd.to_datetime(_df_ct["Timestamp"], errors="coerce")
    _df_ct = _df_ct.tz_localize("America/Denver")
    _df_ct = _df_ct.tz_convert("America/Los_Angeles")
    _df_ct.drop(columns="Timestamp", inplace=True)
    _df_ct = _df_ct.rename(columns={"subject_id": "user-id"})
    _df_ct["user-id"] = _df_ct["user-id"].str.replace("-", "", regex=False)

    # fixme the SBBCAT_ALL and the Room.csv file do not contain the same info
    # a = _df_room.groupby("user-id")["Room"].count()
    # b = _df_ct.groupby("user-id")["user-id"].count()
    # result = pd.concat([a, b], axis=1, join="inner")
    # print(result.query("`user-id` != Room").to_markdown())

    _df_ct["Date"] = _df_ct.index.date.astype("str")
    _df_ct["Session"] = ["Aft" if x > 13 else "Mor" for x in _df_ct.index.hour]
    # _df_ct = _df_ct.reset_index().merge(_df_room, on=["user-id", "Date", "Session"])
    # _df_ct.set_index("Time_Stamp", inplace=True)

    # calculate avg env variables over 5 minutes, down sample the data
    df_env_ds = _df_env.resample("5 min").mean().round(2)
    # df_env_ds = df_env_ds
    # for room in _df_env["room"].unique():
    #     df_room = _df_env[_df_env["room"] == room]
    #     # calculate average reading at different heights
    #     df_room = df_room.groupby(df_room.index).mean()
    #     df_room = df_room.resample("5T").mean()
    #     df_room["Room"] = room
    #     df_room = df_room.dropna(subset=["ta"])
    #     # resample data at 5-minute interval
    #     df_env_ds = pd.concat([df_env_ds, df_room])

    # all_env_timestamps = df_env_ds.index.unique()
    # for room in df_env_ds["Room"].unique():
    #     df_room = df_env_ds[df_env_ds["Room"] == room]
    #     missing_timestamps = [x for x in all_env_timestamps if x not in df_room.index]
    #     missing_data = df_env_ds[df_env_ds.index.isin(missing_timestamps)].copy()
    #     missing_data["Room"] = room
    #     df_env_ds = pd.concat([df_env_ds, missing_data])

    df_env_ds = df_env_ds.dropna(subset=["ta"])
    # df_env_ds = df_env_ds.drop(columns=["sensor-id"])

    # # boxplot showing the room temperatures
    # series = df_env_ds.groupby([df_env_ds.index.date, "Room"])["ta"].mean()
    # df_tmp_rooms = df_env_ds.copy()
    # df_tmp_rooms["Time"] = df_tmp_rooms.index.hour
    # df_tmp_rooms["Date"] = df_tmp_rooms.index.date
    # df_tmp_rooms = df_tmp_rooms.query("9 <= Time <= 17").query(
    #     "Time <= 12 | Time >= 14"
    # )
    # df_tmp_rooms["Session"] = [
    #     "Aft" if x > 12 else "Mor" for x in df_tmp_rooms.index.hour
    # ]
    # df_tmp_rooms.groupby(["Date", "Session"])["ta"].mean()
    # plot = sns.catplot(
    #     x="Date",
    #     y="ta",
    #     data=df_tmp_rooms.sort_index(),
    #     hue="Room",
    #     col="Session",
    #     kind="box",
    # )
    # plt.xticks(rotation=45)
    # plot.set_xticklabels(rotation=45)
    # plt.grid(axis="both", color="0.95")
    # plt.tight_layout()

    # # calculate pmv
    # pmv_a, set_a = [], []
    # for ix, row in df_env_ds.iterrows():
    #     pass
    #     # pmv_a.append(pmv(row["ta"], row["tr"], row["va"], row["rh"], 1.2, 0.57))
    #     # set_a.append(set_tmp(row["ta"], row["tr"], row["va"], row["rh"], 1.2, 0.57))
    #
    # df_env_ds["pmv"] = pmv_a
    # df_env_ds["set"] = set_a

    # users_ct_not_q = [
    #     x for x in _df_ct["user-id"].unique() if x not in _df_q["user-id"].unique()
    # ]
    # users_q_not_ct = [
    #     x for x in _df_q["user-id"].unique() if x not in _df_ct["user-id"].unique()
    # ]
    # if users_ct_not_q != users_q_not_ct:
    #     print("userid differ from Qualtrics and Google databases")

    # import pebl data
    df_pebl = pd.read_csv(
        os.path.join(folder_processed_files, "all_pebl_data.csv"),
        index_col="date time",
    )
    df_pebl = df_pebl.rename(
        columns={
            "user_id": "user-id",
            "session_date": "Date",
            "date time": "time",
        }
    )
    # df_pebl = df_pebl.drop(columns=["...1", "Unnamed: 0"])
    df_pebl["Date"] = df_pebl.Date.astype("str")
    df_pebl.index = pd.to_datetime(df_pebl.index, errors="coerce")
    df_pebl = df_pebl.tz_localize("America/Los_Angeles")
    # df_pebl.loc[(df_pebl["user-id"] == "942G08"), ["user-id"]] = "942G03"
    # df_pebl = df_pebl.merge(
    #     _df_room.drop_duplicates(subset=["Date", "user-id"]),
    #     on=["user-id", "Date"],
    #     how="left",
    #     validate="m:1",
    # )
    # fix issue with these users
    # df_pebl.loc[
    #     (df_pebl["user-id"] == "242B28")
    #     & (df_pebl["Date"] == "2021-11-02")
    #     & (pd.to_datetime(df_pebl["time"]).dt.hour < 12),
    #     ["Session"],
    # ] = "Mor"
    # df_pebl.loc[
    #     (df_pebl["user-id"] == "242B28")
    #     & (df_pebl["Date"] == "2021-11-02")
    #     & (pd.to_datetime(df_pebl["time"]).dt.hour > 12),
    #     ["Session"],
    # ] = "Aft"
    # df_pebl.loc[
    #     (df_pebl["user-id"] == "551D13")
    #     & (df_pebl["Date"] == "2021-11-02")
    #     & (pd.to_datetime(df_pebl["time"]).dt.hour < 12),
    #     ["Session"],
    # ] = "Mor"
    # df_pebl.loc[
    #     (df_pebl["user-id"] == "551D13")
    #     & (df_pebl["Date"] == "2021-11-02")
    #     & (pd.to_datetime(df_pebl["time"]).dt.hour > 12),
    #     ["Session"],
    # ] = "Aft"
    # df_pebl["time"] = [
    #     "10:30:00" if x == "Mor" else "15:30:00" for x in df_pebl.Session
    # ]
    # df_pebl.index = pd.to_datetime(df_pebl["Date"] + " " + df_pebl.time.astype("str"))

    # merge tsv with cognitive test
    _df_ct_q = pd.DataFrame()
    _df_pebl_env = pd.DataFrame()
    for _user in _df_q["user-id"].unique():
        df_q_user = _df_q[_df_q["user-id"] == _user]
        df_ct_user = _df_ct[_df_ct["user-id"] == _user]
        df_ct_user = df_ct_user.tz_convert("America/Los_Angeles")
        df_q_user = df_q_user.tz_convert("America/Los_Angeles")
        df_env_ds = df_env_ds.tz_convert("America/Los_Angeles")
        df_pebl_user = df_pebl[df_pebl["user-id"] == _user]
        df_m = pd.merge_asof(
            df_ct_user.sort_index(),
            df_q_user.sort_index().drop(columns=["Date", "Session", "user-id"]),
            tolerance=pd.Timedelta("1 hour"),
            left_index=True,
            right_index=True,
        )
        df_m = pd.merge_asof(
            df_m.sort_index(),
            df_env_ds.sort_index(),
            left_index=True,
            right_index=True,
            tolerance=pd.Timedelta("30 min"),
            # by="Room",
        )
        df_pb = pd.merge_asof(
            df_pebl_user.sort_index(),
            df_env_ds.sort_index(),
            left_index=True,
            right_index=True,
            tolerance=pd.Timedelta("30 min"),
            # by="Room",
        )
        _df_ct_q = pd.concat([_df_ct_q, df_m])
        _df_pebl_env = pd.concat([_df_pebl_env, df_pb])

    _df_ct_q = _df_ct_q.sort_values("TP", ascending=False)

    _df_ct_q.to_pickle(
        os.path.join(folder_processed_files, "df_ct_q.pkl.gzip"), compression="gzip"
    )

    _df_pebl_env.to_pickle(
        os.path.join(folder_processed_files, "df_pebl_env.pkl.gzip"), compression="gzip"
    )

    return _df_ct_q, _df_pebl_env, _df_env


def tab_demographics():
    # read onboarding data
    _df_onb = pd.read_csv(
        os.path.join(folder_processed_files, "Onboarding.csv"),
        index_col="TimeStamp",
    )
    _df_onb["Age"] = 2021 - _df_onb["YoB"]
    _df_onb["Height"] /= 100
    _df_onb["Sex"] = _df_onb["Sex"].str.capitalize()
    _df_onb["BMI"] = _df_onb["Weight"] / _df_onb["Height"] ** 2

    df_lat = (
        _df_onb.groupby(["Sex"])[["Age", "Height", "Weight", "BMI"]]
        .mean()
        .rename(
            columns={
                "Age": "Age (y)",
                "Height": "Height (m)",
                "Weight": "Weight (kg)",
                "BMI": "BMI (kg/m$^2$)",
            }
        )
    )
    df_lat["Count"] = _df_onb.groupby(["Sex"]).count()["ID"]
    format_dict = {}
    for col in df_lat.columns:
        format_dict[col] = "{:.1f}"
    format_dict["Count"] = "{:.0f}"

    try:
        styled = df_lat.reset_index().style.format(format_dict).hide()
    except AttributeError:
        styled = df_lat.reset_index().style.format(format_dict).hide_index()
    styled.to_latex(
        "manuscript/src/tables/demographics.tex",
        caption="Participants information.",
        position_float="centering",
        multicol_align="|c|",
        hrules=True,
        label="tab:demographics",
        column_format="cccccc",
    )


def tab_env_conditions():
    df_summary = df_env[
        (
            (df_env.index.time >= datetime.time(9, 30))
            & (df_env.index.time <= datetime.time(11, 30))
        )
        | (
            (df_env.index.time >= datetime.time(14, 30))
            & (df_env.index.time <= datetime.time(16, 30))
        )
    ].copy()

    df_summary["Session"] = ["Aft" if x.hour > 12 else "Mor" for x in df_summary.index]
    df_summary["Date"] = df_summary.index.date.astype("str")

    # sns.boxplot(x="Date", y="ta", hue="Session", data=df_summary)

    df_condition = (
        df_ct_q.groupby(["Date", "Session", "Condition"])["ta"].mean().reset_index()
    )
    df_condition["Date"] = df_condition["Date"].astype("str")

    df_summary = pd.merge(
        df_summary,
        df_condition[["Date", "Condition", "Session"]],
        on=["Date", "Session"],
        how="left",
        validate="m:1",
    )

    variables = ["ta", "tr", "rh", "va", "co2"]

    for gb in ["Condition", ["room", "Condition"]]:
        test = df_summary.groupby(gb)[variables].describe().round(1)

        df_table = pd.DataFrame()
        for col in variables:
            _ = (
                test[col]["mean"].astype(str)
                + " ("
                + test[col]["std"].astype(str)
                + ")"
            )
            df_table = pd.concat([df_table, _], axis=1)
        df_table.columns = variables

        df_table = (
            df_table.rename(columns={x: map_col_names[x] + "" for x in variables})
            .transpose()
            .reset_index()
            .rename(columns={"index": "Variable"})
        )
        df_table.style.format({"Condition": "{:.0f}"}).hide(axis="index").to_latex(
            f"manuscript/src/tables/environmental_conditions_{len(gb)}.tex",
            caption=(
                "Environmental conditions measured and logged during the experiment "
                "[mean (SD)]."
            ),
            position_float="centering",
            multicol_align="|c|",
            hrules=True,
            label="tab:environmental",
            column_format="ccccccccccccccccc",
        )


def tab_pebl_results_summary(best_performers=False):
    # create a table with the summary from the PEBL test
    df_pebl_table = df_pebl_env.copy()

    if best_performers:
        # get the best performers
        mean_ct_score = df_ct_q["Result"].mean()
        mean_score_users = df_ct_q.groupby("user-id")["Result"].mean()
        best_performers_ids = mean_score_users[mean_score_users > mean_ct_score].index

        df_pebl_table = df_pebl_table[
            df_pebl_table["user-id"].isin(best_performers_ids)
        ]

    # get the condition for that session
    df_condition = (
        df_ct_q.groupby(["Date", "Session", "Condition"])["ta"].mean().reset_index()
    )
    df_condition["Date"] = df_condition["Date"].astype("str")
    df_condition["Condition"] = df_condition["Condition"].astype("int").astype("str")
    df_pebl_table["Date"] = df_pebl_table.index.date.astype("str")

    # add the condition information to the PEBL data
    df_pebl_table = pd.merge(
        df_pebl_table,
        df_condition[["Date", "Condition", "Session"]],
        on=["Date", "Session"],
        how="left",
        validate="m:1",
    )

    # repeated measurement ANOVA
    rm_anova = []
    for test in df_pebl_table.test.unique():
        df_test = df_pebl_table.query("test == @test")
        for parameter in df_test.parameter.unique():
            df_param = df_test.query("parameter == @parameter")
            df_param = df_param.rename(columns={"user-id": "id"})
            users_full_dataset = (
                df_param.groupby(["id", "Condition"])[["value"]]
                .mean()
                .unstack("Condition")
                .dropna()
                .index
            )
            res = AnovaRM(
                data=df_param[df_param["id"].isin(users_full_dataset)],
                depvar="value",
                subject="id",
                within=["Condition"],
                aggregate_func="mean",
            ).fit()
            rm_anova.append(
                [
                    test,
                    parameter,
                    f"{res.anova_table.values[0][-1]:.2f} ({len(users_full_dataset)})",
                ]
            )
    users_full_dataset = (
        df_ct_q.groupby(["user-id", "Condition"])[["Result"]]
        .mean()
        .unstack("Condition")
        .dropna()
        .index
    )
    df_anova_rm = df_ct_q[df_ct_q["user-id"].isin(users_full_dataset)].copy()
    df_anova_rm["id"] = df_anova_rm["user-id"]
    res = AnovaRM(
        data=df_anova_rm,
        depvar="Result",
        within=["Condition"],
        subject="id",
        aggregate_func="mean",
    ).fit()
    rm_anova.append(
        [
            "SBBCAT",
            "Result",
            f"{res.anova_table.values[0][-1]:.2f} ({len(users_full_dataset)})",
        ]
    )
    df_rm_anova = pd.DataFrame(data=rm_anova, columns=["test", "parameter", "p"])

    # calculate main statistical variables
    df_pebl_table = (
        df_pebl_table.groupby(["test", "parameter", "metric", "Condition"])["value"]
        .describe()[["50%", "25%", "75%", "count"]]
        .round(1)
    )

    _df = df_ct_q.copy()
    _df["test"] = "SBBCAT"
    _df["parameter"] = "Result"
    _df["metric"] = "N/A"
    _df["Condition"] = _df["Condition"].astype("int").astype("str")
    df_pebl_table = pd.concat(
        [
            df_pebl_table,
            (
                _df.groupby(["test", "parameter", "metric", "Condition"])["Result"]
                .describe()[["50%", "25%", "75%", "count"]]
                .round(1)
            ),
        ]
    )

    df_pebl_table.unstack(level=["Condition"]).to_csv(
        f"{folder_results}/pebl_summary.csv"
    )

    # column containing 50%, std, and count for each condition
    df_pebl_table["Conditions"] = (
        df_pebl_table["50%"].astype(str)
        + " ("
        + df_pebl_table["25%"].astype(str)
        + ", "
        + df_pebl_table["75%"].astype(str)
        + ", "
        + df_pebl_table["count"].astype(str)
        + ")"
    )

    df_pebl_table = df_pebl_table.unstack(level=["Condition"])

    # column depicting change in scores
    df_pebl_table["Change"] = (
        "T25: "
        + (
            (
                (df_pebl_table["50%"]["25"] - df_pebl_table["50%"]["23"])
                / df_pebl_table["50%"]["23"]
            )
            * 100
        )
        .round(1)
        .astype(str)
        + " %; "
        + "T27: "
        + (
            (
                (df_pebl_table["50%"]["27"] - df_pebl_table["50%"]["23"])
                / df_pebl_table["50%"]["23"]
            )
            * 100
        )
        .round(1)
        .astype(str)
        + " %; "
        + "T29: "
        + (
            (
                (df_pebl_table["50%"]["29"] - df_pebl_table["50%"]["23"])
                / df_pebl_table["50%"]["23"]
            )
            * 100
        )
        .round(1)
        .astype(str)
        + " %"
    )

    # style the table
    df_table = df_pebl_table[["Conditions", "Change"]].reset_index().copy()

    # drop entries not needed for the analysis
    number_of_correct = "number of correct"
    drop_entries = [
        {"test": "bcst", "parameter": "sd reaction time"},
        {"test": "flanker", "parameter": number_of_correct},
        {"test": "flanker", "parameter": "percentage of correct"},
        {"test": "match_to_sample", "parameter": number_of_correct},
        {"test": "paired", "parameter": number_of_correct},
        {"test": "tol", "parameter": number_of_correct},
    ]
    for drop_entry in drop_entries:
        index_to_remove = df_table[
            (df_table.parameter == drop_entry["parameter"])
            & (df_table.test == drop_entry["test"])
        ].index
        df_table = df_table.drop(index_to_remove)

    df_table.columns = [" ".join(col).strip() for col in df_table.columns.values]
    df_table = pd.merge(
        df_table,
        df_rm_anova,
        on=["test", "parameter"],
    )
    sort_tests = [
        "SBBCAT",
        "bcst",
        "corsi",
        "flanker",
        "match_to_sample",
        "paired",
        "tol",
        "ptrails",
    ]
    df_table = df_table.sort_values(
        by="test", key=lambda x: x.map({x: ix for ix, x in enumerate(sort_tests)})
    )
    if best_performers:
        df_table.to_latex("manuscript/src/tables/pebl_best_performers.tex")
    else:
        df_table.to_latex("manuscript/src/tables/pebl_all.tex", index=False)
    df_styled = (
        df_table.set_index("test").style.format()
        # .applymap(lambda v: "opacity: 20%;" if (v > 0.05) else None, subset="p")
    )

    # export the table
    if best_performers:
        dfi.export(df_styled, "manuscript/src/figures/pebl_best_performers.png")
    else:
        dfi.export(df_styled, "manuscript/src/figures/pebl_all.png")


def fig_tsv_tp_vs_t(t="ta"):
    # analyse thermal sensation votes
    f, axs = plt.subplots(1, 3, constrained_layout=True)
    sns.regplot(
        x=t,
        y="TSV",
        data=df_ct_q,
        ax=axs[0],
        scatter_kws={"s": 7, "alpha": 0.5, "color": grayscale[3]},
        line_kws={"color": secondary_core[0]},
    )
    axs[0].grid(axis="x")
    sns.boxplot(x="TP", y=t, data=df_ct_q, ax=axs[1], palette=tc_palette, fliersize=0)
    sns.swarmplot(x="TP", y=t, data=df_ct_q, ax=axs[1], color=".25", size=2)
    if t == "ta":
        axs[0].set_xticks([23, 25, 27, 29])
        axs[1].set_yticks([23, 25, 27, 29])
    sns.boxplot(x="TP", y="TSV", data=df_ct_q, ax=axs[2], palette=tc_palette)
    for ax in axs:
        ax.set_xlabel(map_col_names[ax.get_xlabel()])
        ax.set_ylabel(map_col_names[ax.get_ylabel()])

    plt.savefig(f"manuscript/src/figures/tsv_tp_vs_{t}.png")
    # sns.regplot(x=t, y="TSV", data=df_ct_q, x_estimator=np.mean, x_bins=4)


def fig_tsv_tp_vs_tsk():
    # analyse thermal sensation votes
    f, axs = plt.subplots(1, 1, constrained_layout=True)
    sns.regplot(
        x="t-sk",
        y="TSV",
        data=df_ct_q,
        ax=axs,
        scatter_kws={"s": 7, "alpha": 0.5, "color": grayscale[3]},
        line_kws={"color": secondary_core[0]},
    )
    axs.grid(axis="x")
    _df = df_ct_q[["t-sk", "TSV"]].dropna()
    reg = LinearRegression().fit(_df["t-sk"].values.reshape(-1, 1), _df["TSV"])
    r2 = reg.score(_df["t-sk"].values.reshape(-1, 1), _df["TSV"])

    plt.title(
        f"R2 = {r2:.2f}; TSV ="
        f" {reg.coef_[0]:.2f} {map_col_names[axs.get_xlabel()]} {reg.intercept_:.2f}"
    )
    print(30.5 * reg.coef_[0] + reg.intercept_)
    print(34.8 * reg.coef_[0] + reg.intercept_)
    axs.set_xlabel(map_col_names[axs.get_xlabel()])
    axs.set_ylabel(map_col_names[axs.get_ylabel()])
    plt.savefig("manuscript/src/figures/tsv_tp_vs_t-sk.png")


def ct_vs_tsv_tp():
    f, axs = plt.subplots(1, 3, constrained_layout=True)
    df_ct_q["TSV_grouped"] = pd.cut(
        df_ct_q["TSV"],
        [-4, -1.5, -0.5, 0.5, 1.5, 4],
        labels=["c", "sc", "n", "sw", "w"],
    )
    sns.boxplot(
        x="TSV", y="Result", data=df_ct_q, ax=axs[0], palette=tsv_palette, fliersize=0
    )
    count_points = df_ct_q.groupby("TSV")["TSV"].count()
    for ix, index in enumerate(count_points.index):
        axs[0].text(ix, 100, f"# {count_points[index]}", ha="center")
    sns.swarmplot(x="TSV", y="Result", data=df_ct_q, ax=axs[0], color=".25", size=2)

    sns.boxplot(
        x="TSV_grouped",
        y="Result",
        data=df_ct_q,
        ax=axs[1],
        palette=["#06A6EE", "#31CAA8", "#31CAA8", "#31CAA8", "#FF412C", "#FF412C"],
        fliersize=0,
    )
    # print(df_ct_q.anova(dv="Result", between=["TSV_grouped"]).round(3))
    count_points = df_ct_q.groupby("TSV_grouped")["TSV_grouped"].count()
    for ix, index in enumerate(count_points.index):
        axs[1].text(ix, 95, f"# {count_points[index]}", ha="center")
    sns.swarmplot(
        x="TSV_grouped", y="Result", data=df_ct_q, ax=axs[1], color=".25", size=2
    )
    add_stat_annotation(
        axs[1],
        data=df_ct_q,
        x="TSV_grouped",
        y="Result",
        order=["c", "sc", "n", "sw", "w"],
        box_pairs=[
            ("c", "n"),
            ("n", "w"),
        ],
        test="Mann-Whitney",
        text_format="star",
        loc="outside",
        verbose=2,
    )

    sns.boxplot(
        x="TP",
        y="Result",
        data=df_ct_q.sort_values("TP", ascending=False),
        ax=axs[2],
        palette=list(reversed(tc_palette)),
        fliersize=0,
    )
    add_stat_annotation(
        axs[2],
        data=df_ct_q.sort_values("TP", ascending=False),
        x="TP",
        y="Result",
        order=df_ct_q.sort_values("TP", ascending=False).TP.unique()[:-1],
        box_pairs=[
            ("Warmer", "Cooler"),
        ],
        test="Mann-Whitney",
        text_format="star",
        loc="outside",
        verbose=2,
    )
    count_points = df_ct_q.groupby("TP")["TP"].count().sort_index(ascending=False)
    for ix, index in enumerate(count_points.index):
        axs[2].text(ix, 95, f"# {count_points[index]}", ha="center")
    sns.swarmplot(
        x="TP",
        y="Result",
        data=df_ct_q.sort_values("TP", ascending=False),
        ax=axs[2],
        color=".25",
        size=2,
    )
    plt.savefig("manuscript/src/figures/ct_vs_tsv_tp.png")


def ct_vs_t():
    f, axs = plt.subplots(1, 2, constrained_layout=True)
    sns.boxplot(x="Condition", y="Result", data=df_ct_q, ax=axs[0], palette=grayscale)
    count_points = df_ct_q.groupby("Condition")["Condition"].count()
    for ix, index in enumerate(count_points.index):
        axs[0].text(ix, 100, f"# {count_points[index]}", ha="center")
    users_full_dataset = (
        df_ct_q.groupby(["user-id", "Condition"])[["Result"]]
        .mean()
        .unstack("Condition")
        .dropna()
        .index
    )
    df_anova_rm = df_ct_q[df_ct_q["user-id"].isin(users_full_dataset)].copy()
    df_anova_rm["id"] = df_anova_rm["user-id"]
    rm_anova = AnovaRM(
        data=df_anova_rm,
        depvar="Result",
        within=["Condition"],
        subject="id",
        aggregate_func="mean",
    ).fit()
    axs[0].text(
        0.5,
        0.3,
        f"p={rm_anova.anova_table.values[0][-1]:.2f}",
        ha="center",
        va="center",
        transform=axs[0].transAxes,
    )
    sns.regplot(
        x="ta",
        y="Result",
        data=df_ct_q,
        ax=axs[1],
        scatter_kws={"s": 7, "alpha": 0.5, "color": grayscale[3]},
        line_kws={"color": secondary_core[0]},
        robust=True,
    )
    axs[1].set_xticks([23, 25, 27, 29])
    for ax in axs:
        ax.set_xlabel(map_col_names[ax.get_xlabel()])
        ax.set_ylabel(map_col_names[ax.get_ylabel()])

    plt.savefig("manuscript/src/figures/ct_vs_ta.png")


def ct_analysis():
    # perform the repeated measures ANOVA, I can only use a balanced dataset
    users_full_dataset = (
        df_ct_q.set_index(["user-id", "Condition"])[["Result"]]
        .unstack("Condition")
        .dropna()
        .index
    )
    df_anova_rm = df_ct_q[df_ct_q["user-id"].isin(users_full_dataset)].copy()
    df_anova_rm["id"] = df_anova_rm["user-id"]
    res = AnovaRM(
        data=df_anova_rm,
        depvar="Result",
        subject="id",
        within=["Condition"],
    ).fit()
    print(res)
    res.anova_table.style.to_latex(
        "manuscript/src/tables/ct_condition_anova_rm.tex",
        caption=(
            "Results of the repeated measurement ANOVA used to determine statistically"
            " significant difference between CT scores at different conditions"
        ),
        position_float="centering",
        hrules=True,
        label="tab:ct_condition_anova_rm",
        column_format="ccccc",
    )

    # perform repeated measures linear regression source https://www.statsmodels.org/stable/examples/notebooks/generated/mixed_lm_example.html
    md = smf.mixedlm(
        "Result ~ ta",
        df_ct_q,
        groups=df_ct_q["user-id"],
        re_formula="~ta",
    )
    # fit a model with two random effects for each person: a random intercept and slope
    # Each person may have a different baseline score, as well as changing differently
    mdf = md.fit()
    print(mdf.summary())
    write_path = "manuscript/src/tables/ct_condition_lmem.tex"
    with open(write_path, "w") as f:
        f.write(mdf.summary().as_latex())


def learning_effect_ct_test():
    df_learning_ct = pd.DataFrame()
    for user in df_ct_q["user-id"].unique():
        df_user = (
            df_ct_q.query("`user-id` in @user").sort_index().reset_index().reset_index()
        )
        df_learning_ct = pd.concat([df_learning_ct, df_user])
    _, axs = plt.subplots(1, 3, constrained_layout=True)
    sns.boxenplot(x="index", y="Result", data=df_learning_ct, ax=axs[0])
    # sns.swarmplot(x="index", y="Result", data=df_learning_ct, ax=axs[0], color=".1")
    # sns.boxenplot(x="index", y="Condition", data=df_learning_ct, ax=axs[1])
    sns.swarmplot(x="index", y="ta", data=df_learning_ct, ax=axs[1], color=".1")
    df_learning_ct.groupby(["index", "Condition"])["Result"].count().unstack()
    summary_df = (
        df_learning_ct.groupby(["index", "Condition"])["Result"].count().unstack()
    )
    axs[2].table(
        cellText=summary_df.values,
        colLabels=summary_df.columns,
        rowLabels=summary_df.index,
        loc="center",
    )
    axs[2].axis("off")
    axs[2].axis("tight")
    plt.show()

    print(
        df_learning_ct.rm_anova(
            dv="ta",
            within=["index"],
            subject="user-id",
        ).to_markdown()
    )


def learning_effect_pebl_test():
    for test in df_pebl_env.test.unique():
        df_test = df_pebl_env.query("test in @test")
        parameters = df_test.parameter.unique()
        _, axs = plt.subplots(
            len(parameters), 1, constrained_layout=True, sharex=True, figsize=(7, 7)
        )
        axs = axs.flatten()
        for ix, parameter in enumerate(parameters):
            df_learning_ct = pd.DataFrame()
            for user in df_test["user-id"].unique():
                df_user = df_test.query("`user-id` in @user")
                df_user = df_user[df_user.parameter == parameter]
                df_user = df_user.sort_index().reset_index(drop=True).reset_index()
                df_learning_ct = pd.concat([df_learning_ct, df_user])
            sns.boxenplot(x="index", y="value", data=df_learning_ct, ax=axs[ix])
            bot = df_test.query("parameter == @parameter")["value"].quantile(0.1)
            top = df_test.query("parameter == @parameter")["value"].quantile(0.9)
            axs[ix].set(ylim=(bot, top), title=parameter)
        plt.savefig(f"manuscript/src/figures/pebl_learn_{test}.png")


def ct_vs_demographic():
    # CT by different demographic variables
    df_onb = pd.read_csv(
        os.path.join(folder_processed_files, "Onboarding.csv"),
    )
    df_onb["Height"] /= 100
    df_onb["BMI"] = df_onb["Weight"] / df_onb["Height"] ** 2
    df_onb["BMI_word"] = pd.cut(
        df_onb["BMI"],
        [0, 18.5, 24.9, 29.9, 39.9],
        labels=["under", "normal", "over", "obese"],
    )
    df_test = pd.merge(df_ct_q, df_onb, left_on="user-id", right_on="ID")
    variables = ["Sex", "BMI_word", "SchoolLevel"]
    for var in variables:
        plt.subplots(constrained_layout=True)
        sns.boxplot(
            x=var,
            y="Result",
            data=df_test,
        )
        sns.swarmplot(
            x=var,
            y="Result",
            data=df_test,
            color=".25",
            size=2,
        )
        plt.xticks(rotation=25)
        plt.savefig(f"manuscript/src/figures/ct_by_{var}.png")
    male = df_test.query("Sex == 'MALE'")
    female = df_test.query("Sex == 'FEMALE'")
    ttest_ind(male["Result"], female["Result"], equal_var=False)


def ct_stacked_bar_perceptions():
    for variable in [
        "TSV",
        "TP",
        "Acceptability_1",
        "Acceptability_2",
        "Acceptability_3",
        "Acceptability_4",
    ]:
        partial = (
            df_ct_q.groupby(["Condition", variable])["ta"].count().unstack(variable)
        )
        total = (
            df_ct_q.groupby(["Condition", variable])["ta"]
            .count()
            .unstack(variable)
            .sum(axis=1)
        )
        df_plot = partial.div(total, 0) * 100
        if variable.split("_")[0] == "Acceptability":
            df_plot = df_plot[sorted_col["Acceptability"]]
        try:
            df_plot.columns = [
                " ".join(x.capitalize().split(" ")[:2]) for x in df_plot.columns
            ]
        except AttributeError:
            pass
        df_plot = df_plot.reset_index()

        df_plot.plot(
            x="Condition",
            kind="bar",
            stacked=True,
            mark_right=True,
            width=0.95,
            rot=0,
            legend=False,
            cmap="viridis",
        )

        plt.legend(
            bbox_to_anchor=(0, 1.15, 1, 0.2),
            loc="lower left",
            mode="expand",
            borderaxespad=0,
            ncol=3,
        )
        plt.ylabel("Percentage (%)")
        plt.title(variable)
        plt.tight_layout()
        plt.savefig(f"manuscript/src/figures/ct_stacked_bar_{variable}.png")


if __name__ == "__main__":
    sns.set_style("whitegrid")

    plt.rc("axes.spines", top=False, right=False, left=False)
    plt.rcParams["font.family"] = "sans-serif"
    plt.rcParams["figure.figsize"] = (7, 3)
    font = {"size": 10}

    mpl.rc("font", **font)

    folder_results = os.path.join(os.getcwd(), "processed/results")

    df_ct_q, df_pebl_env, df_env = import_merge_df(load_saved_file=False)

    # # analyse PEBL results only when we were recording them
    # df_pebl_env = df_pebl_env[df_pebl_env["Recording"] == "YES"]

if __name__ == "check_anomalies":
    # analyse cognitive performance vs temperature
    # check if the condition column matches the real data
    plt.figure()
    sns.regplot(x="ta", y="Condition", data=df_ct_q)

    # check when Condition and Ta significantly differ
    df_difference = df_ct_q.set_index("user-id", append=True)[["Condition", "ta"]]
    df_difference["delta"] = df_difference.diff(axis=1)["ta"].abs()
    df_difference.sort_values("delta", ascending=False).round(2).to_csv(
        f"{folder_results}/difference_condition_measured.csv"
    )
    df_difference = df_difference[df_difference["ta"].isna()].reset_index()
    print(df_difference.to_markdown())

    # # code to check why there were some missing values in the PEBL
    # values = (
    #     df_pebl_table.query("test == 'tol' & parameter == 'total time'")
    #     .groupby(["user-id", "metric", "ta"])["value"]
    #     .describe()[["count"]]
    #     .unstack(level=["ta"])
    # )
    # print(values.loc[values["count"].isna().any(axis=1).values].stack("ta"))
    # df_pebl_table.query(
    #     "test == 'tol' & parameter == 'total time' & `user-id` == '021J24'"
    # )["ta"]
    # df_pebl_table.query(
    #     "test == 'tol' & parameter == 'total time' & `user-id` == '141I13'"
    # )["ta"]
    # df_env_ds.query("index.dt.date == datetime.date(2021, 10, 28)").between_time("11:30", "11:50")

if __name__ == "__plot__":
    plt.close("all")

    # table showing the temperature in the rooms

    # table depicting the participants demographics
    # tab_demographics()

    # summarize environmental conditions in the rooms
    # tab_env_conditions()

    # table depicting how participants performed in the PEBL test as function of ta
    tab_pebl_results_summary(best_performers=False)
    tab_pebl_results_summary(best_performers=True)

    # figure showing how the TSV and TP varied as function of temperature
    fig_tsv_tp_vs_t()
    fig_tsv_tp_vs_tsk()

    # SBB CT vs TSV and TP votes
    ct_vs_tsv_tp()

    # SBB CT vs perception in a stacked bar chart
    ct_stacked_bar_perceptions()

    for person in df_ct_q["user-id"].unique():
        df_person = df_ct_q.query("`user-id` == @person")
        plt.figure()
        sns.regplot(x="TSV", y="Result", data=df_person, ci=None)
        plt.title(person)

    # ct_vs_demographic()

    # SBB CT vs condition and air temperature
    ct_vs_t()

    ct_analysis()

    # # SBB CT vs condition and ta but filtering out participants with little variation
    # f, axs = plt.subplots(2, 1, constrained_layout=True)
    # df_ct_big_change = df_ct_q.groupby("user-id")["Result"].std()
    # users_high_ct_variation = df_ct_big_change.to_frame().query("`Result` > 5").index
    # _df = df_ct_q.query("`user-id` in @users_high_ct_variation")
    # sns.boxplot(x="Condition", y="Result", data=_df, ax=axs[0])
    # sns.regplot(x="ta", y="Result", data=_df, ax=axs[1])

    # learning effect
    learning_effect_ct_test()

    learning_effect_pebl_test()
