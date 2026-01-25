# =========================================================================
# TEST REFERENCE UI MODULE
# =========================================================================
# UI for Test Reference tab - displays HTML table reference for test selection
# Part of One- and Two-Sample Tests module

create_test_reference_ui <- function(ns) {
  # Replicate HTML table from original app (lines 1556-1660)
  HTML("<style>th, td{padding:10px;}</style><table>
<tbody>
<tr>
<td>&nbsp;</td>
<td colspan= '2 ' rowspan= '2 ' style= 'text-align: center; '><strong>One-Sample Statistics</strong></td>
<td colspan= '4 ' style= 'text-align: center; '><strong>&nbsp;Two-Sample Statistics&nbsp;&nbsp;&nbsp;</strong></td>
</tr>
<tr>
<td><strong>&nbsp;</strong></td>
<td colspan= '2 ' style= 'text-align: center; '><strong>Independent</strong></td>
<td colspan= '2 ' style= 'text-align: center; '><strong>Dependent</strong></td>
</tr>
<tr style= 'border: 1px solid #000000; '>
<td style= 'border: 1px solid #000000; '><strong>Data Type&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; '><strong>Center&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; '><strong>Spread&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; text-align: center; '><strong>Center&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; text-align: center; '><strong>Spread&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; text-align: center; '><strong>Center&nbsp;</strong></td>
<td style= 'border: 1px solid #000000; text-align: center; '><strong>Spread&nbsp;</strong></td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; '><strong>Interval or Ratio</strong></td>
<td style= 'border: 1px solid #000000; '>
<p>Z<sub>X-bar</sub> for &mu;</p>
<p>t for&nbsp;&mu;</p>
</td>
<td style= 'border: 1px solid #000000; '>&nbsp;&chi;<sup>2</sup> for&nbsp;&sigma;<sup>2</sup></td>
<td style= 'border: 1px solid #000000; '>
<p>&nbsp;Z<sub>X-bar1 - X-bar 2</sub> for&nbsp;&mu;<sub>1</sub> -&nbsp;&mu;<sub>2</sub> = 0</p>
<p>t for&nbsp;&mu;<sub>1</sub> -&nbsp;&mu;<sub>2</sub> = 0 (equal &sigma;)</p>
<p>t for&nbsp;&mu;<sub>1</sub> -&nbsp;&mu;<sub>2</sub> = 0 (unequal &sigma;)</p>
</td>
<td style= 'border: 1px solid #000000; '>
<p>F for &sigma;<sub>1</sub><sup>2</sup> =&nbsp;&sigma;<sub>2</sub><sup>2</sup></p>
<p>Levene t or F</p>
<p>t on ADM<sub>(n-1)</sub></p>
</td>
<td style= 'border: 1px solid #000000; '>t for&nbsp;&nbsp;&nbsp;&mu;<sub>1</sub> -&nbsp;&mu;<sub>2</sub> = 0</td>
<td style= 'border: 1px solid #000000; '>t for&nbsp;&nbsp;&sigma;<sub>1</sub><sup>2</sup> =&nbsp;&sigma;<sub>2</sub><sup>2</sup>&nbsp;</td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; '><strong>Nominal</strong></td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '><p>Exact binomial test for proportions</p></td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Two-sample proportion test</p>
<p>&chi;<sup>2</sup> test (2x2 tables)</p>
&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Binomial sign test</p>
<p>McNemar's dependent proportion test&nbsp;</p>
&nbsp;</td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; '><strong>&nbsp;Ordinal</strong></td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Sign test for location (Median test)</p>
<p>Wilcoxon Signed-Ranks test&nbsp;</p>
&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Median test</p>
<p>Wilcoxon-Mann-Whitney U test</p>
<p>Kologorov-Smirnov test</p>
&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Wilcoxon signed-ranks test</p>
<p>Binomial sign test&nbsp;</p>
&nbsp;</td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; '>
<p><strong>Count</strong></p>
<p style= 'text-align: right; '><strong>Poisson&nbsp; &nbsp;</strong></p>
</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>One-sample Poisson exact test&nbsp;&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Two-sample Poisson&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>
<p>Dependent group t-test</p>
<p>Wilcoxon signed-ranks test&nbsp;</p>
&nbsp;</td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; text-align: right; '><strong>Absolute</strong>&nbsp;&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Proper parametric tests&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Proper independent groups test for means and variance&nbsp;&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Dependent group t-test&nbsp;&nbsp;</td>
</tr>
<tr>
<td style= 'border: 1px solid #000000; text-align: right; '><strong>Other Counts&nbsp;&nbsp;&nbsp;</strong></td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Proper nominal data techniques&nbsp;&nbsp;</td>
<td style= 'border: 1px solid #000000; '>Proper independent groups nominal data techniques&nbsp;</td>
<td style= 'border: 1px solid #000000; '>&nbsp;</td>
<td colspan= '2 ' style= 'border: 1px solid #000000; '>Dependent groups nominal data techniques&nbsp;&nbsp;</td>
</tr>
<tr>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
<td>&nbsp;</td>
</tr>
</tbody>
</table>")
}
