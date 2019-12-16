package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpTestReportSearchListDTO
 * @Description 功能描述
 * @Date 2019/6/7 18:53
 **/

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("模糊搜索化验单列表")
public class ErpTestReportSearchListDTO implements Serializable {
    private static final long serialVersionUID = 2708325555826364889L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("编号")
    private String code;

    @ApiModelProperty("化验结果")
    private String result;

}
