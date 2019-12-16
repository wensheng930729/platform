package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpAuxiliaryMaterialConsumptionSearchRQ
 * @Description 功能描述
 * @Date 2019/5/30 14:17
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("辅材消耗搜索请求参数")
public class ErpAuxiliaryMaterialConsumptionSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 公司名称
     */
//    @ApiModelProperty("公司名称")
//    private String companyName;

    @ApiModelProperty("公司名称")
    private Integer companyId;

    /**
     * 炉号
     */
//    @ApiModelProperty("炉号")
//    private String furnaceNumber;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;


}
