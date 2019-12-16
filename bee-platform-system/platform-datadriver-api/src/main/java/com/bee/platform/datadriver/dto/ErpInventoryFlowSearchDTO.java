package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpInventoryFlowSearchDTO
 * @Description 功能描述
 * @Date 2019/5/31 17:13
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存流水根据条件搜索返回信息")
public class ErpInventoryFlowSearchDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("标题数组")
    private String[] title;

    @ApiModelProperty("库存流水返回信息")
    private List<ErpInventoryFlowDTO> dtoList ;

    @ApiModelProperty("合计")
    private BigDecimal sum ;

}
