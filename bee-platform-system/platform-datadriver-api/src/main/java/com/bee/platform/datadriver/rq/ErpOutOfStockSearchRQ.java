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
 * @ClassName ErpOutOfStockSearchRQ
 * @Description 功能描述
 * @Date 2019/5/30 15:11
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("领料出库请求参数")
public class ErpOutOfStockSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 领料出库编号
     */
    @ApiModelProperty("领料出库编号")
    private String code;

    /**
     * 公司名称
     */
//    @ApiModelProperty("公司名称")
//    private String companyName;

    @ApiModelProperty("公司名称")
    private Integer companyId;


    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
    private String materialBatchName;
    /**
     * 炉号
     */
//    @ApiModelProperty("炉号")
//    private String furnaceNumber;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    /**
     * 班次
     */
    @ApiModelProperty("班次")
    private String classes;

    @ApiModelProperty("确认状态(0已保存，1已确认)")
    private Integer state;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;

}
