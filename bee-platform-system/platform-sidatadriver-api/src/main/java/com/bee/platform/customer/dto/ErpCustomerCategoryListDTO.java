package com.bee.platform.customer.dto;

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
@ApiModel("erp客户分类列表返回DTO")
public class ErpCustomerCategoryListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("客户二级分类名称")
    private String name;

    @ApiModelProperty("客户一级分类code")
    private String pcode;

    @ApiModelProperty("客户一级分类名称")
    private String pName;

    @ApiModelProperty("1-启用，0-禁用")
    private Integer status;

}
