 package com.bee.platform.dinas.datadriver.support;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum OperateType {
     ADD("新增"), EDIT("编辑"), DELETE("删除"),SETTLEMENT("结算"),CANCEL_SETTLEMENT("撤销结算");
     private String msg;
 }
