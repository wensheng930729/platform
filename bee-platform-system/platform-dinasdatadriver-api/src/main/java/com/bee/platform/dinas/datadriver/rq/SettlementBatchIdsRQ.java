package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName SettlementBatchIdsRQ
 * @Description 功能描述
 * @Date 2019/8/15 10:41
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("批量结算id集合请求参数")
public class SettlementBatchIdsRQ implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("合同id")
    @NotNull(message = "合同id不能为空")
    private Integer contractOrderId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产品规格id")
    @NotNull(message = "产品规格id不能为空")
    private Integer productSpecId;


    @ApiModelProperty("数量")
    @NotNull(message = "数量不能为空")
    private BigDecimal num;


}
