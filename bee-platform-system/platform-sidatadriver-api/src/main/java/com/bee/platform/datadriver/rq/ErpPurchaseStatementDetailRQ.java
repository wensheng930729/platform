package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单明细请求信息")
public class ErpPurchaseStatementDetailRQ implements Serializable {
    private static final long serialVersionUID = -7432921208266015508L;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("收货重量")
    private BigDecimal srcDryWeight;

    @ApiModelProperty("结算重量")
    private BigDecimal realDryWeight;

    @ApiModelProperty("结算金额")
    private BigDecimal realAmount;

    @ApiModelProperty("结算扣款")
    private BigDecimal realDebit;

    @ApiModelProperty("结算单价")
    private BigDecimal realPrice;

    @ApiModelProperty("结算日期")
    @NotNull(message = "结算日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;
}
