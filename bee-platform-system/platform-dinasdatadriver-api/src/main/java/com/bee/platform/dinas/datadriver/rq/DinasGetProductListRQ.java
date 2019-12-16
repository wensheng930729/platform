package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName DinasGetProductListRQ
 * @Description 功能描述
 * @Date 2019/8/14 11:29
 **/
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("查询产品列表请求参数")
public class DinasGetProductListRQ implements Serializable {
    private static final long serialVersionUID = 1L;
    /**
     * 采购合同id
     */
    @ApiModelProperty("采购合同id")
    @NotNull(message = "采购合同id不能为空")
    private Integer purchaseOrderId;
    /**
     * 销售合同id
     */
    @ApiModelProperty("销售合同id")
    @NotNull(message = "销售合同id不能为空")
    private Integer saleOrderId;

}
