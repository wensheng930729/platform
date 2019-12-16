package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpWarehousingListDTO
 * @Description 功能描述
 * @Date 2019/6/6 14:54
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("用户下成品入库订单列表")
@JsonInclude
public class ErpWarehousingListDTO implements Serializable {

    private static final long serialVersionUID = 2708325555826364889L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 成品入库编号
     */
    @ApiModelProperty("成品入库编号")
    private String code;

    /**
     * 成品入库编号
     */
    @ApiModelProperty("成品入库编号")
    private String contractNo;


}
