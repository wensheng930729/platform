package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 化验类型
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("化验类型列表对象")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ErpTestTypeDTO implements Serializable{

    private static final long serialVersionUID = 552291406982975025L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("化验类别")
    private Integer type;

    @ApiModelProperty("化验类型名称")
    private String name;

    @ApiModelProperty("1-启用,0-停用")
    private Integer status;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("企业名称")
    private String enterpriseName;

}
