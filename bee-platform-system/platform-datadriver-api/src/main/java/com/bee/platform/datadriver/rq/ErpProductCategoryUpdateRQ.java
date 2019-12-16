package com.bee.platform.datadriver.rq;

import com.bee.platform.datadriver.dto.ErpProductCategoryCheckItemsDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("erp产品分类修改更新入参")
public class ErpProductCategoryUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品类别id")
    private Integer id;

    @ApiModelProperty("产品类别编号")
    private String code;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;

    @ApiModelProperty("产品检测属性")
    private List<ErpProductCategoryCheckItemsDTO> checkItems;



}
