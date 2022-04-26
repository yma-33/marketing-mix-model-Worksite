import math

# Custom utility functions
import utils.generate_data_utils as gd
import pandas as pd
import pytest
from mmpac import *
import os


class TestReadInCoefficients:
    def setup_method(self):
        """
        Pull in all the outputs of the generate_data function
        """

        self.comb_coeff_file_path = "tests/test_combinationCoeffs.csv"
        self.output_comb = ["lob", "state", "segment"]
        self.stakeholder_bounds = pd.read_csv(
            "tests/test_stakeholder_bounds.csv")
        self.alpha_output_names, self.alpha, self.impression_names, \
            self.S, self.ch_camp_comb = gd.read_in_coefficients(
                self.comb_coeff_file_path, self.output_comb,
                "Q3_2020_BUDGET_MAX", self.stakeholder_bounds, get_file_from="file_path",
            )

    def test_alpha(self):
        """
        Test that alpha, impression_names, and alpha_output_names are just a deconstructed
        versions of combinationCoeffs.csv. In this test we re-create combinationCoeffs
        out of alpha, impression_names, and alpha_output_names
        """
        # Reconstruct the combinationCoeffs file
        reconstructed_coeffs = (
            pd.DataFrame(
                self.alpha,
                columns=self.impression_names,
                index=self.alpha_output_names,
            )
            .unstack()
            .reset_index()
            .dropna()
        )
        reconstructed_coeffs.columns = ["input", "output", "coeff"]

        # Pull in actual combinationCoeffs file
        actual_coeffs = pd.read_csv(self.comb_coeff_file_path)
        # remove all negative coeffs
        actual_coeffs = actual_coeffs[actual_coeffs["coeff"] > 0]
        compare_df = pd.merge(
            actual_coeffs,
            reconstructed_coeffs,
            on=["input", "output"],
            how="left",
            indicator="Exist",
        )

        assert all(
            compare_df["coeff_x"] == compare_df["coeff_y"]
        ), "alpha doesn't match combinationCoeffs"

    def test_S(self):
        """
        Test that S matrix is 1 ONLY when there is a non "nan" alpha
        otherwise the S matrix should always be zero
        """
        test_S_values = []
        for i in range(len(self.alpha)):
            for j in range(len(self.alpha[i])):
                if math.isnan(self.alpha[i][j]):
                    if self.S[i][j] == 0:
                        test_S_values += [True]
                    else:
                        test_S_values += [False]
                else:
                    if self.S[i][j] == 1:
                        test_S_values += [True]
                    else:
                        test_S_values += [False]

        assert all(test_S_values)
