package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName ErpProductListRQ
 * @Description erp产品列表查询rq
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品列表查询入参")
public class ErpProductListRQ implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品类别")
    private Integer category;

    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;


}
