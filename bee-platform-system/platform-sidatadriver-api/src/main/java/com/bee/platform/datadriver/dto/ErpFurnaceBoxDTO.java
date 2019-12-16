package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
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

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("炉子下拉框使用dto")
@JsonInclude
public class ErpFurnaceBoxDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("炉子id")
    private Integer id;

    @ApiModelProperty("炉子名称")
    private String name;

}
