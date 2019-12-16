package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 采购结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("批量结算请求参数")
public class SettlementBatchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("ids")
    @NotEmpty(message = "ids集合不能为空")
    private List<SettlementBatchIdsRQ> ids;

    @ApiModelProperty("结算单价")
    @NotNull(message = "结算单价不能为空")
    @Min(value = 0, message = "结算单价大于等于0")
    private BigDecimal settlementUnitPrice;
   
    @ApiModelProperty("结算总价")
    @NotNull(message = "结算总价不能为空")
    @Min(value = 0, message = "结算总价大于等于0")
    private BigDecimal settlementSumPrice;




}
