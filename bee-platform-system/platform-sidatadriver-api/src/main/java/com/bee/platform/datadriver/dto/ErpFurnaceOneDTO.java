package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 炉子档案
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("炉子回显返回信息dto")
@JsonInclude
public class ErpFurnaceOneDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer orgId;
    /**
     * 名称
     */
    @ApiModelProperty("名称")
    private String name;
    /**
     * 状态
     */
    @ApiModelProperty("状态")
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    @ApiModelProperty("是否删除 0未删除 1删除")
    private Integer deleted;


}
