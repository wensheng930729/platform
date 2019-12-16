package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName ErpProductBatchDTO
 * @Description erp产品批次返回dto
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品批次返回dto")
public class ErpProductBatchDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产品批次分类项名称")
    private String batchName;

}
