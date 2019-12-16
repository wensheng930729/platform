package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 期初库存主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("根据id查询期初库存返回信息")
@JsonInclude
public class ErpOpeningInventoryOrderDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 期初库存编号
     */
    @ApiModelProperty("期初库存编号")
    private String code;
    /**
     * 期初日期
     */
    @ApiModelProperty("期初日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openingInventoryTime;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;


    /**
     * 确认状态(0已保存，1已确认)
     */
    @ApiModelProperty("确认状态(0已保存，1已确认)")
    private Integer state;

    @ApiModelProperty("期初库存明细返回信息")
    List<ErpOpeningInventoryOrderDetailDTO> detailDTOList;



}
