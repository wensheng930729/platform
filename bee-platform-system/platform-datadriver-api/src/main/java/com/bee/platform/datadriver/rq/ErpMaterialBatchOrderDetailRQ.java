package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 料批明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("料批请求参数")
public class ErpMaterialBatchOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 料批主表id
     */
    @ApiModelProperty("料批主表id")
    @NotNull(message = "料批主表id不能为空")
    private Integer materialBatchOrderId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
//    @NotEmpty(message = "产品名称不能为空")
    private String productName;
    /**
     * 数量
     */
    @ApiModelProperty("数量")
    @NotNull(message = "数量不能为空")
    @Min(value = 0,message = "数量不能小于0")
    private BigDecimal number;


}
