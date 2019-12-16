package com.bee.platform.datadriver.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 成品入库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("成品入库搜索请求参数")
public class ErpWarehousingOrderSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 成品入库编号
     */
    @ApiModelProperty("成品入库编号")
    private String code;
    /**
     * 料批名称
     */
    @ApiModelProperty("料批名称")
    private String materialBatchName;
    /**
     * 炉号
     */
    @ApiModelProperty("炉号id")
    private Integer furnaceId;
    /**
     * 班次
     */
    @ApiModelProperty("班次")
    private String classes;


    @ApiModelProperty("产成品id")
    private Integer productId;

    /**
     * 确认状态(0已保存，1已确认)
     */
//    @ApiModelProperty("确认状态(0已保存，1已确认)")
//    private String state;

    @ApiModelProperty("公司名称id")
    private Integer companyId;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;


    private List<Integer> list;



}
