import pytest
from utils.preprocess_utils import *


class TestVerticaData:
    def setup_method(self):
        """
        Pull in the vertica dataframes and test that their key unique
        values are as expected
        """
        self.impression_table, self.conversion_table, self.factor_table = get_vertica_data()
        self.valid_states = ["AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT",
                             "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID",
                             "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                             "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
                             "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR",
                             "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI",
                             "VT", "WA", "WI", "WV", "WY"]
        self.valid_channels = ["Email", "Twitter", "GDN", "Cinema", "Snapchat",
                               "Print_Newspaper", "TV", "Pinterest", "Digital_Video",
                               "Facebook", "OOH", "Audio_Terrestrial", "Print_Magazine",
                               "Instagram", "Direct_Mail", "SEM",
                               "Audio_Streaming", "Display", "Newsletter"]
        self.valid_campaigns = ["Unknown", "Sponsorship", "Direct Response",
                                "Brand", "Non-MMFA BU", "Cultural Moment"]
        self.valid_lobs = ["Ann", "Di", "Invs", "Life", "Ltc"]

    def test_impression_table_channels(self):
        impr_df = self.impression_table
        impr_df_channels = list(impr_df["channel"].unique())
        assert (all(item in self.valid_channels for item in impr_df_channels)
                ), "invalid channel"

    def test_impression_table_campaigns(self):
        impr_df = self.impression_table
        impr_df_campaigns = list(impr_df["campaign"].unique())
        assert (all(item in self.valid_campaigns for item in impr_df_campaigns)
                ), "invalid campaign"

    def test_impression_table_states(self):
        impr_df = self.impression_table
        impr_df_states = list(impr_df["state"].unique())
        assert (all(item in self.valid_states for item in impr_df_states)
                ), "invalid impression state"

    def test_conversion_table_lob(self):
        convr_df = self.conversion_table
        convr_df_lob = list(convr_df["LOB"].unique())
        assert (all(item in self.valid_lobs for item in convr_df_lob)
                ), "invalid lob"

    def test_conversion_table_lob(self):
        convr_df = self.conversion_table
        convr_df_state = list(convr_df["state"].unique())
        assert (all(item in self.valid_states for item in convr_df_state)
                ), "invalid conversion state"
