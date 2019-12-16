package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @Classname ErpPurchaseStatementReportReturnDTO
 * @Description 采购结算单详情中验收情况信息返回DTO
 * @Date 2019/6/6 14:16
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单详情中验收情况信息返回DTO")
public class ErpPurchaseStatementReportReturnDTO implements Serializable {
    private static final long serialVersionUID = -2007202604311615751L;

    @ApiModelProperty("化验单标题")
    private List<Map<String, String>> title;

    @ApiModelProperty("各项检测值")
    private List<Map<String,String>> reportItemList;

    @ApiModelProperty("合计信息")
    private Map<String,BigDecimal> total;
}
