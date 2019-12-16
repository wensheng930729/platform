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
 * @ClassName ErpFurnacaAddRQ
 * @Description erp炉子增加入参
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp炉子增加入参")
public class ErpFurnacaAddRQ implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("所属企业id")
    private Integer orgId;

    @ApiModelProperty("炉子名称")
    private String name;

    @ApiModelProperty("炉子状态")
    private Integer status;

}
