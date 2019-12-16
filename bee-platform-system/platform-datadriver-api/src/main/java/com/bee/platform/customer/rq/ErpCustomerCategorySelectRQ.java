package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 客户分类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("erp客户分类列表查询RQ")
public class ErpCustomerCategorySelectRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("客户二级分类")
    private Integer id;

    @ApiModelProperty("客户一级分类")
    private String pcode;

    @ApiModelProperty("状态 1-启用，0-禁用")
    private Integer status;

}
