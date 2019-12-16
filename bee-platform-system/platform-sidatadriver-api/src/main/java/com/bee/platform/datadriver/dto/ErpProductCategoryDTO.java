package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp产品分类返回DTO当前登录人企业下所有分类")
public class ErpProductCategoryDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品类别名称")
    private String name;

}
