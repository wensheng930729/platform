package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Classname ErpPurchaseStatementTestReportDTO
 * @Description 采购结算单验收情况信息
 * @Date 2019/6/6 9:48
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("采购结算单验收情况信息")
public class ErpPurchaseStatementTestReportDTO implements Serializable {
    private static final long serialVersionUID = 2409861988736884171L;

    @ApiModelProperty("采购订单id")
    private Integer receiptOrderId;

    @ApiModelProperty("到厂数量")
    private BigDecimal num;

    @ApiModelProperty("化验单号")
    private String code;

}
