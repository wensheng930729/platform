package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
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
@ApiModel("erp客户分类添加DTO")
public class ErpCustomerCategoryAddRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("客户一级分类")
    @NotNull(message = "客户一级分类不能为空")
    private String pcode;

    @ApiModelProperty("客户二级分类")
    @NotNull(message = "客户二级分类不能为空")
    private String  name;

    @ApiModelProperty("状态 1-启用，0-禁用")
    @NotNull(message = "状态不能为空")
    private Integer status;

}
