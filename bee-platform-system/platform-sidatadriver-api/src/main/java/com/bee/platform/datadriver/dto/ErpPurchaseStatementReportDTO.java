package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @Classname ErpPurchaseStatementReportDTO
 * @Description 采购结算单详情中验收情况信息
 * @Date 2019/6/6 14:16
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单详情中验收情况信息")
public class ErpPurchaseStatementReportDTO implements Serializable {
    private static final long serialVersionUID = -2007202604311615751L;

    @ApiModelProperty("化验单id")
    private Integer id;

    @ApiModelProperty("化验单号")
    private String code;

    @ApiModelProperty("到厂日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("到厂数量")
    private BigDecimal num;

    @ApiModelProperty("化验单结果信息")
    private String result;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("化验单项目信息")
    private List<ErpTestReportDetailDTO> reportList;
}
