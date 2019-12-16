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
@ApiModel("炉子列表返回信息")
@JsonInclude
public class ErpFurnaceListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("企业名称")
    private String orgName;

    @ApiModelProperty("名称")
    private String name;

    @ApiModelProperty("状态1启用 0禁用")
    private Integer status;





}
