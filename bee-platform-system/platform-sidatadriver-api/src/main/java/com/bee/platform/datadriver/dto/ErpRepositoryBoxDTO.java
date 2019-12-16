package com.bee.platform.datadriver.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 仓库档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@Accessors(chain = true)
@ApiModel("仓库列表下拉框返回信息")
public class ErpRepositoryBoxDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 名称
     */
    @ApiModelProperty("仓库名称")
    private String name;




}
